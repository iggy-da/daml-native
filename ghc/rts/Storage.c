/* -----------------------------------------------------------------------------
 * $Id: Storage.c,v 1.67 2002/07/17 09:21:51 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Storage manager front end
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "Stats.h"
#include "Hooks.h"
#include "BlockAlloc.h"
#include "MBlock.h"
#include "Weak.h"
#include "Sanity.h"
#include "Arena.h"

#include "Storage.h"
#include "Schedule.h"
#include "OSThreads.h"
#include "StoragePriv.h"

#include "RetainerProfile.h"	// for counting memory blocks (memInventory)

#include <stdlib.h>
#include <string.h>

#ifdef darwin_TARGET_OS
#include <mach-o/getsect.h>
unsigned long macho_etext = 0;
unsigned long macho_edata = 0;

static void macosx_get_memory_layout(void)
{
  struct segment_command *seg;

  seg = getsegbyname("__TEXT");
  macho_etext = seg->vmaddr + seg->vmsize;
  seg = getsegbyname("__DATA");
  macho_edata = seg->vmaddr + seg->vmsize;
}
#endif

StgClosure    *caf_list         = NULL;

bdescr *small_alloc_list;	/* allocate()d small objects */
bdescr *large_alloc_list;	/* allocate()d large objects */
bdescr *pinned_object_block;    /* allocate pinned objects into this block */
nat alloc_blocks;		/* number of allocate()d blocks since GC */
nat alloc_blocks_lim;		/* approximate limit on alloc_blocks */

StgPtr alloc_Hp    = NULL;	/* next free byte in small_alloc_list */
StgPtr alloc_HpLim = NULL;	/* end of block at small_alloc_list   */

generation *generations;	/* all the generations */
generation *g0;			/* generation 0, for convenience */
generation *oldest_gen;		/* oldest generation, for convenience */
step *g0s0;			/* generation 0, step 0, for convenience */

lnat total_allocated = 0;	/* total memory allocated during run */

/*
 * Storage manager mutex:  protects all the above state from
 * simultaneous access by two STG threads.
 */
#ifdef SMP
Mutex sm_mutex = INIT_MUTEX_VAR;
#endif

/*
 * Forward references
 */
static void *stgAllocForGMP   (size_t size_in_bytes);
static void *stgReallocForGMP (void *ptr, size_t old_size, size_t new_size);
static void  stgDeallocForGMP (void *ptr, size_t size);

void
initStorage( void )
{
  nat g, s;
  step *stp;
  generation *gen;

#if defined(darwin_TARGET_OS)
    macosx_get_memory_layout();
#endif

    /* Sanity check to make sure we are able to make the distinction
     * between closures and infotables
     */
  if (!LOOKS_LIKE_GHC_INFO(&stg_BLACKHOLE_info)) {
    barf("LOOKS_LIKE_GHC_INFO+ is incorrectly defined");
    exit(0);
  }
  if (LOOKS_LIKE_GHC_INFO(&stg_dummy_ret_closure)) {
    barf("LOOKS_LIKE_GHC_INFO- is incorrectly defined");
    exit(0);
  }
  if (LOOKS_LIKE_STATIC_CLOSURE(&stg_BLACKHOLE_info)) {
    barf("LOOKS_LIKE_STATIC_CLOSURE- is incorrectly defined");
    exit(0);
  }
  if (!LOOKS_LIKE_STATIC_CLOSURE(&stg_dummy_ret_closure)) {
    barf("LOOKS_LIKE_STATIC_CLOSURE+ is incorrectly defined");
    exit(0);
  }

  if (RtsFlags.GcFlags.maxHeapSize != 0 &&
      RtsFlags.GcFlags.heapSizeSuggestion > 
      RtsFlags.GcFlags.maxHeapSize) {
    RtsFlags.GcFlags.maxHeapSize = RtsFlags.GcFlags.heapSizeSuggestion;
  }

  if (RtsFlags.GcFlags.maxHeapSize != 0 &&
      RtsFlags.GcFlags.minAllocAreaSize > 
      RtsFlags.GcFlags.maxHeapSize) {
      prog_belch("maximum heap size (-M) is smaller than minimum alloc area size (-A)");
      exit(1);
  }

  initBlockAllocator();
  
#if defined(SMP)
  initCondition(&sm_mutex);
#endif

  /* allocate generation info array */
  generations = (generation *)stgMallocBytes(RtsFlags.GcFlags.generations 
					     * sizeof(struct _generation),
					     "initStorage: gens");

  /* Initialise all generations */
  for(g = 0; g < RtsFlags.GcFlags.generations; g++) {
    gen = &generations[g];
    gen->no = g;
    gen->mut_list = END_MUT_LIST;
    gen->mut_once_list = END_MUT_LIST;
    gen->collections = 0;
    gen->failed_promotions = 0;
    gen->max_blocks = 0;
  }

  /* A couple of convenience pointers */
  g0 = &generations[0];
  oldest_gen = &generations[RtsFlags.GcFlags.generations-1];

  /* Allocate step structures in each generation */
  if (RtsFlags.GcFlags.generations > 1) {
    /* Only for multiple-generations */

    /* Oldest generation: one step */
    oldest_gen->n_steps = 1;
    oldest_gen->steps = 
      stgMallocBytes(1 * sizeof(struct _step), "initStorage: last step");

    /* set up all except the oldest generation with 2 steps */
    for(g = 0; g < RtsFlags.GcFlags.generations-1; g++) {
      generations[g].n_steps = RtsFlags.GcFlags.steps;
      generations[g].steps  = 
	stgMallocBytes (RtsFlags.GcFlags.steps * sizeof(struct _step),
			"initStorage: steps");
    }
    
  } else {
    /* single generation, i.e. a two-space collector */
    g0->n_steps = 1;
    g0->steps = stgMallocBytes (sizeof(struct _step), "initStorage: steps");
  }

  /* Initialise all steps */
  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
    for (s = 0; s < generations[g].n_steps; s++) {
      stp = &generations[g].steps[s];
      stp->no = s;
      stp->blocks = NULL;
      stp->n_blocks = 0;
      stp->gen = &generations[g];
      stp->gen_no = g;
      stp->hp = NULL;
      stp->hpLim = NULL;
      stp->hp_bd = NULL;
      stp->scan = NULL;
      stp->scan_bd = NULL;
      stp->large_objects = NULL;
      stp->n_large_blocks = 0;
      stp->new_large_objects = NULL;
      stp->scavenged_large_objects = NULL;
      stp->n_scavenged_large_blocks = 0;
      stp->is_compacted = 0;
      stp->bitmap = NULL;
    }
  }
  
  /* Set up the destination pointers in each younger gen. step */
  for (g = 0; g < RtsFlags.GcFlags.generations-1; g++) {
    for (s = 0; s < generations[g].n_steps-1; s++) {
      generations[g].steps[s].to = &generations[g].steps[s+1];
    }
    generations[g].steps[s].to = &generations[g+1].steps[0];
  }
  
  /* The oldest generation has one step and it is compacted. */
  if (RtsFlags.GcFlags.compact) {
      if (RtsFlags.GcFlags.generations == 1) {
	  belch("WARNING: compaction is incompatible with -G1; disabled");
      } else {
	  oldest_gen->steps[0].is_compacted = 1;
      }
  }
  oldest_gen->steps[0].to = &oldest_gen->steps[0];

  /* generation 0 is special: that's the nursery */
  generations[0].max_blocks = 0;

  /* G0S0: the allocation area.  Policy: keep the allocation area
   * small to begin with, even if we have a large suggested heap
   * size.  Reason: we're going to do a major collection first, and we
   * don't want it to be a big one.  This vague idea is borne out by 
   * rigorous experimental evidence.
   */
  g0s0 = &generations[0].steps[0];

  allocNurseries();

  weak_ptr_list = NULL;
  caf_list = NULL;
   
  /* initialise the allocate() interface */
  small_alloc_list = NULL;
  large_alloc_list = NULL;
  alloc_blocks = 0;
  alloc_blocks_lim = RtsFlags.GcFlags.minAllocAreaSize;

  /* Tell GNU multi-precision pkg about our custom alloc functions */
  mp_set_memory_functions(stgAllocForGMP, stgReallocForGMP, stgDeallocForGMP);

#if defined(SMP)
  initMutex(&sm_mutex);
#endif

  IF_DEBUG(gc, statDescribeGens());
}

void
exitStorage (void)
{
    stat_exit(calcAllocated());
}

/* -----------------------------------------------------------------------------
   CAF management.

   The entry code for every CAF does the following:
     
      - builds a CAF_BLACKHOLE in the heap
      - pushes an update frame pointing to the CAF_BLACKHOLE
      - invokes UPD_CAF(), which:
          - calls newCaf, below
	  - updates the CAF with a static indirection to the CAF_BLACKHOLE
      
   Why do we build a BLACKHOLE in the heap rather than just updating
   the thunk directly?  It's so that we only need one kind of update
   frame - otherwise we'd need a static version of the update frame too.

   newCaf() does the following:
       
      - it puts the CAF on the oldest generation's mut-once list.
        This is so that we can treat the CAF as a root when collecting
	younger generations.

   For GHCI, we have additional requirements when dealing with CAFs:

      - we must *retain* all dynamically-loaded CAFs ever entered,
        just in case we need them again.
      - we must be able to *revert* CAFs that have been evaluated, to
        their pre-evaluated form.

      To do this, we use an additional CAF list.  When newCaf() is
      called on a dynamically-loaded CAF, we add it to the CAF list
      instead of the old-generation mutable list, and save away its
      old info pointer (in caf->saved_info) for later reversion.

      To revert all the CAFs, we traverse the CAF list and reset the
      info pointer to caf->saved_info, then throw away the CAF list.
      (see GC.c:revertCAFs()).

      -- SDM 29/1/01

   -------------------------------------------------------------------------- */

void
newCAF(StgClosure* caf)
{
  /* Put this CAF on the mutable list for the old generation.
   * This is a HACK - the IND_STATIC closure doesn't really have
   * a mut_link field, but we pretend it has - in fact we re-use
   * the STATIC_LINK field for the time being, because when we
   * come to do a major GC we won't need the mut_link field
   * any more and can use it as a STATIC_LINK.
   */
  ACQUIRE_SM_LOCK;

  if (is_dynamically_loaded_rwdata_ptr((StgPtr)caf)) {
      ((StgIndStatic *)caf)->saved_info  = (StgInfoTable *)caf->header.info;
      ((StgIndStatic *)caf)->static_link = caf_list;
      caf_list = caf;
  } else {
      ((StgIndStatic *)caf)->saved_info = NULL;
      ((StgMutClosure *)caf)->mut_link = oldest_gen->mut_once_list;
      oldest_gen->mut_once_list = (StgMutClosure *)caf;
  }

  RELEASE_SM_LOCK;

#ifdef PAR
  /* If we are PAR or DIST then  we never forget a CAF */
  { globalAddr *newGA;
    //belch("<##> Globalising CAF %08x %s",caf,info_type(caf));
    newGA=makeGlobal(caf,rtsTrue); /*given full weight*/
    ASSERT(newGA);
  } 
#endif /* PAR */
}

/* -----------------------------------------------------------------------------
   Nursery management.
   -------------------------------------------------------------------------- */

void
allocNurseries( void )
{ 
#ifdef SMP
  {
    Capability *cap;
    bdescr *bd;

    g0s0->blocks = NULL;
    g0s0->n_blocks = 0;
    for (cap = free_capabilities; cap != NULL; cap = cap->link) {
      cap->r.rNursery = allocNursery(NULL, RtsFlags.GcFlags.minAllocAreaSize);
      cap->r.rCurrentNursery = cap->r.rNursery;
      for (bd = cap->r.rNursery; bd != NULL; bd = bd->link) {
	bd->u.back = (bdescr *)cap;
      }
    }
    /* Set the back links to be equal to the Capability,
     * so we can do slightly better informed locking.
     */
  }
#else /* SMP */
  g0s0->blocks      = allocNursery(NULL, RtsFlags.GcFlags.minAllocAreaSize);
  g0s0->n_blocks    = RtsFlags.GcFlags.minAllocAreaSize;
  g0s0->to_blocks   = NULL;
  g0s0->n_to_blocks = 0;
  MainCapability.r.rNursery        = g0s0->blocks;
  MainCapability.r.rCurrentNursery = g0s0->blocks;
  /* hp, hpLim, hp_bd, to_space etc. aren't used in G0S0 */
#endif
}
      
void
resetNurseries( void )
{
  bdescr *bd;
#ifdef SMP
  Capability *cap;
  
  /* All tasks must be stopped */
  ASSERT(n_free_capabilities == RtsFlags.ParFlags.nNodes);

  for (cap = free_capabilities; cap != NULL; cap = cap->link) {
    for (bd = cap->r.rNursery; bd; bd = bd->link) {
      bd->free = bd->start;
      ASSERT(bd->gen_no == 0);
      ASSERT(bd->step == g0s0);
      IF_DEBUG(sanity,memset(bd->start, 0xaa, BLOCK_SIZE));
    }
    cap->r.rCurrentNursery = cap->r.rNursery;
  }
#else
  for (bd = g0s0->blocks; bd; bd = bd->link) {
    bd->free = bd->start;
    ASSERT(bd->gen_no == 0);
    ASSERT(bd->step == g0s0);
    IF_DEBUG(sanity,memset(bd->start, 0xaa, BLOCK_SIZE));
  }
  MainCapability.r.rNursery = g0s0->blocks;
  MainCapability.r.rCurrentNursery = g0s0->blocks;
#endif
}

bdescr *
allocNursery (bdescr *tail, nat blocks)
{
  bdescr *bd;
  nat i;

  // Allocate a nursery: we allocate fresh blocks one at a time and
  // cons them on to the front of the list, not forgetting to update
  // the back pointer on the tail of the list to point to the new block.
  for (i=0; i < blocks; i++) {
    // @LDV profiling
    /*
      processNursery() in LdvProfile.c assumes that every block group in
      the nursery contains only a single block. So, if a block group is
      given multiple blocks, change processNursery() accordingly.
     */
    bd = allocBlock();
    bd->link = tail;
    // double-link the nursery: we might need to insert blocks
    if (tail != NULL) {
	tail->u.back = bd;
    }
    bd->step = g0s0;
    bd->gen_no = 0;
    bd->flags = 0;
    bd->free = bd->start;
    tail = bd;
  }
  tail->u.back = NULL;
  return tail;
}

void
resizeNursery ( nat blocks )
{
  bdescr *bd;
  nat nursery_blocks;

#ifdef SMP
  barf("resizeNursery: can't resize in SMP mode");
#endif

  nursery_blocks = g0s0->n_blocks;
  if (nursery_blocks == blocks) {
    return;
  }

  else if (nursery_blocks < blocks) {
    IF_DEBUG(gc, fprintf(stderr, "Increasing size of nursery to %d blocks\n", 
			 blocks));
    g0s0->blocks = allocNursery(g0s0->blocks, blocks-nursery_blocks);
  } 

  else {
    bdescr *next_bd;
    
    IF_DEBUG(gc, fprintf(stderr, "Decreasing size of nursery to %d blocks\n", 
			 blocks));

    bd = g0s0->blocks;
    while (nursery_blocks > blocks) {
	next_bd = bd->link;
	next_bd->u.back = NULL;
	nursery_blocks -= bd->blocks; // might be a large block
	freeGroup(bd);
	bd = next_bd;
    }
    g0s0->blocks = bd;
    // might have gone just under, by freeing a large block, so make
    // up the difference.
    if (nursery_blocks < blocks) {
	g0s0->blocks = allocNursery(g0s0->blocks, blocks-nursery_blocks);
    }
  }
  
  g0s0->n_blocks = blocks;
  ASSERT(countBlocks(g0s0->blocks) == g0s0->n_blocks);
}

/* -----------------------------------------------------------------------------
   The allocate() interface

   allocate(n) always succeeds, and returns a chunk of memory n words
   long.  n can be larger than the size of a block if necessary, in
   which case a contiguous block group will be allocated.
   -------------------------------------------------------------------------- */

StgPtr
allocate( nat n )
{
  bdescr *bd;
  StgPtr p;

  ACQUIRE_SM_LOCK;

  TICK_ALLOC_HEAP_NOCTR(n);
  CCS_ALLOC(CCCS,n);

  /* big allocation (>LARGE_OBJECT_THRESHOLD) */
  /* ToDo: allocate directly into generation 1 */
  if (n >= LARGE_OBJECT_THRESHOLD/sizeof(W_)) {
    nat req_blocks =  (lnat)BLOCK_ROUND_UP(n*sizeof(W_)) / BLOCK_SIZE;
    bd = allocGroup(req_blocks);
    dbl_link_onto(bd, &g0s0->large_objects);
    bd->gen_no  = 0;
    bd->step = g0s0;
    bd->flags = BF_LARGE;
    bd->free = bd->start;
    /* don't add these blocks to alloc_blocks, since we're assuming
     * that large objects are likely to remain live for quite a while
     * (eg. running threads), so garbage collecting early won't make
     * much difference.
     */
    alloc_blocks += req_blocks;
    RELEASE_SM_LOCK;
    return bd->start;

  /* small allocation (<LARGE_OBJECT_THRESHOLD) */
  } else if (small_alloc_list == NULL || alloc_Hp + n > alloc_HpLim) {
    if (small_alloc_list) {
      small_alloc_list->free = alloc_Hp;
    }
    bd = allocBlock();
    bd->link = small_alloc_list;
    small_alloc_list = bd;
    bd->gen_no = 0;
    bd->step = g0s0;
    bd->flags = 0;
    alloc_Hp = bd->start;
    alloc_HpLim = bd->start + BLOCK_SIZE_W;
    alloc_blocks++;
  }

  p = alloc_Hp;
  alloc_Hp += n;
  RELEASE_SM_LOCK;
  return p;
}

lnat
allocated_bytes( void )
{
  return (alloc_blocks * BLOCK_SIZE_W - (alloc_HpLim - alloc_Hp));
}

/* ---------------------------------------------------------------------------
   Allocate a fixed/pinned object.

   We allocate small pinned objects into a single block, allocating a
   new block when the current one overflows.  The block is chained
   onto the large_object_list of generation 0 step 0.

   NOTE: The GC can't in general handle pinned objects.  This
   interface is only safe to use for ByteArrays, which have no
   pointers and don't require scavenging.  It works because the
   block's descriptor has the BF_LARGE flag set, so the block is
   treated as a large object and chained onto various lists, rather
   than the individual objects being copied.  However, when it comes
   to scavenge the block, the GC will only scavenge the first object.
   The reason is that the GC can't linearly scan a block of pinned
   objects at the moment (doing so would require using the
   mostly-copying techniques).  But since we're restricting ourselves
   to pinned ByteArrays, not scavenging is ok.

   This function is called by newPinnedByteArray# which immediately
   fills the allocated memory with a MutableByteArray#.
   ------------------------------------------------------------------------- */

StgPtr
allocatePinned( nat n )
{
    StgPtr p;
    bdescr *bd = pinned_object_block;

    ACQUIRE_SM_LOCK;
    
    TICK_ALLOC_HEAP_NOCTR(n);
    CCS_ALLOC(CCCS,n);

    // If the request is for a large object, then allocate()
    // will give us a pinned object anyway.
    if (n >= LARGE_OBJECT_THRESHOLD/sizeof(W_)) {
	RELEASE_SM_LOCK;
	return allocate(n);
    }

    // we always return 8-byte aligned memory.  bd->free must be
    // 8-byte aligned to begin with, so we just round up n to
    // the nearest multiple of 8 bytes.
    if (sizeof(StgWord) == 4) {
	n = (n+1) & ~1;
    }

    // If we don't have a block of pinned objects yet, or the current
    // one isn't large enough to hold the new object, allocate a new one.
    if (bd == NULL || (bd->free + n) > (bd->start + BLOCK_SIZE_W)) {
	pinned_object_block = bd = allocBlock();
	dbl_link_onto(bd, &g0s0->large_objects);
	bd->gen_no = 0;
	bd->step   = g0s0;
	bd->flags  = BF_LARGE;
	bd->free   = bd->start;
	alloc_blocks++;
    }

    p = bd->free;
    bd->free += n;
    RELEASE_SM_LOCK;
    return p;
}

/* -----------------------------------------------------------------------------
   Allocation functions for GMP.

   These all use the allocate() interface - we can't have any garbage
   collection going on during a gmp operation, so we use allocate()
   which always succeeds.  The gmp operations which might need to
   allocate will ask the storage manager (via doYouWantToGC()) whether
   a garbage collection is required, in case we get into a loop doing
   only allocate() style allocation.
   -------------------------------------------------------------------------- */

static void *
stgAllocForGMP (size_t size_in_bytes)
{
  StgArrWords* arr;
  nat data_size_in_words, total_size_in_words;
  
  /* round up to a whole number of words */
  data_size_in_words  = (size_in_bytes + sizeof(W_) + 1) / sizeof(W_);
  total_size_in_words = sizeofW(StgArrWords) + data_size_in_words;
  
  /* allocate and fill it in. */
  arr = (StgArrWords *)allocate(total_size_in_words);
  SET_ARR_HDR(arr, &stg_ARR_WORDS_info, CCCS, data_size_in_words);
  
  /* and return a ptr to the goods inside the array */
  return(BYTE_ARR_CTS(arr));
}

static void *
stgReallocForGMP (void *ptr, size_t old_size, size_t new_size)
{
    void *new_stuff_ptr = stgAllocForGMP(new_size);
    nat i = 0;
    char *p = (char *) ptr;
    char *q = (char *) new_stuff_ptr;

    for (; i < old_size; i++, p++, q++) {
	*q = *p;
    }

    return(new_stuff_ptr);
}

static void
stgDeallocForGMP (void *ptr STG_UNUSED, 
		  size_t size STG_UNUSED)
{
    /* easy for us: the garbage collector does the dealloc'n */
}

/* -----------------------------------------------------------------------------
 * Stats and stuff
 * -------------------------------------------------------------------------- */

/* -----------------------------------------------------------------------------
 * calcAllocated()
 *
 * Approximate how much we've allocated: number of blocks in the
 * nursery + blocks allocated via allocate() - unused nusery blocks.
 * This leaves a little slop at the end of each block, and doesn't
 * take into account large objects (ToDo).
 * -------------------------------------------------------------------------- */

lnat
calcAllocated( void )
{
  nat allocated;
  bdescr *bd;

#ifdef SMP
  Capability *cap;

  /* All tasks must be stopped.  Can't assert that all the
     capabilities are owned by the scheduler, though: one or more
     tasks might have been stopped while they were running (non-main)
     threads. */
  /*  ASSERT(n_free_capabilities == RtsFlags.ParFlags.nNodes); */

  allocated = 
    n_free_capabilities * RtsFlags.GcFlags.minAllocAreaSize * BLOCK_SIZE_W
    + allocated_bytes();

  for (cap = free_capabilities; cap != NULL; cap = cap->link) {
    for ( bd = cap->r.rCurrentNursery->link; bd != NULL; bd = bd->link ) {
      allocated -= BLOCK_SIZE_W;
    }
    if (cap->r.rCurrentNursery->free < cap->r.rCurrentNursery->start 
	+ BLOCK_SIZE_W) {
      allocated -= (cap->r.rCurrentNursery->start + BLOCK_SIZE_W)
	- cap->r.rCurrentNursery->free;
    }
  }

#else /* !SMP */
  bdescr *current_nursery = MainCapability.r.rCurrentNursery;

  allocated = (g0s0->n_blocks * BLOCK_SIZE_W) + allocated_bytes();
  for ( bd = current_nursery->link; bd != NULL; bd = bd->link ) {
    allocated -= BLOCK_SIZE_W;
  }
  if (current_nursery->free < current_nursery->start + BLOCK_SIZE_W) {
    allocated -= (current_nursery->start + BLOCK_SIZE_W)
      - current_nursery->free;
  }
#endif

  total_allocated += allocated;
  return allocated;
}  

/* Approximate the amount of live data in the heap.  To be called just
 * after garbage collection (see GarbageCollect()).
 */
extern lnat 
calcLive(void)
{
  nat g, s;
  lnat live = 0;
  step *stp;

  if (RtsFlags.GcFlags.generations == 1) {
    live = (g0s0->n_to_blocks - 1) * BLOCK_SIZE_W + 
      ((lnat)g0s0->hp_bd->free - (lnat)g0s0->hp_bd->start) / sizeof(W_);
    return live;
  }

  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
    for (s = 0; s < generations[g].n_steps; s++) {
      /* approximate amount of live data (doesn't take into account slop
       * at end of each block).
       */
      if (g == 0 && s == 0) { 
	  continue; 
      }
      stp = &generations[g].steps[s];
      live += (stp->n_large_blocks + stp->n_blocks - 1) * BLOCK_SIZE_W;
      if (stp->hp_bd != NULL) {
	  live += ((lnat)stp->hp_bd->free - (lnat)stp->hp_bd->start) 
	      / sizeof(W_);
      }
    }
  }
  return live;
}

/* Approximate the number of blocks that will be needed at the next
 * garbage collection.
 *
 * Assume: all data currently live will remain live.  Steps that will
 * be collected next time will therefore need twice as many blocks
 * since all the data will be copied.
 */
extern lnat 
calcNeeded(void)
{
    lnat needed = 0;
    nat g, s;
    step *stp;
    
    for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
	for (s = 0; s < generations[g].n_steps; s++) {
	    if (g == 0 && s == 0) { continue; }
	    stp = &generations[g].steps[s];
	    if (generations[g].steps[0].n_blocks +
		generations[g].steps[0].n_large_blocks 
		> generations[g].max_blocks
		&& stp->is_compacted == 0) {
		needed += 2 * stp->n_blocks;
	    } else {
		needed += stp->n_blocks;
	    }
	}
    }
    return needed;
}

/* -----------------------------------------------------------------------------
   Debugging

   memInventory() checks for memory leaks by counting up all the
   blocks we know about and comparing that to the number of blocks
   allegedly floating around in the system.
   -------------------------------------------------------------------------- */

#ifdef DEBUG

void
memInventory(void)
{
  nat g, s;
  step *stp;
  bdescr *bd;
  lnat total_blocks = 0, free_blocks = 0;

  /* count the blocks we current have */

  for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
    for (s = 0; s < generations[g].n_steps; s++) {
      stp = &generations[g].steps[s];
      total_blocks += stp->n_blocks;
      if (RtsFlags.GcFlags.generations == 1) {
	/* two-space collector has a to-space too :-) */
	total_blocks += g0s0->n_to_blocks;
      }
      for (bd = stp->large_objects; bd; bd = bd->link) {
	total_blocks += bd->blocks;
	/* hack for megablock groups: they have an extra block or two in
	   the second and subsequent megablocks where the block
	   descriptors would normally go.
	*/
	if (bd->blocks > BLOCKS_PER_MBLOCK) {
	  total_blocks -= (MBLOCK_SIZE / BLOCK_SIZE - BLOCKS_PER_MBLOCK)
	                  * (bd->blocks/(MBLOCK_SIZE/BLOCK_SIZE));
	}
      }
    }
  }

  /* any blocks held by allocate() */
  for (bd = small_alloc_list; bd; bd = bd->link) {
    total_blocks += bd->blocks;
  }
  for (bd = large_alloc_list; bd; bd = bd->link) {
    total_blocks += bd->blocks;
  }

#ifdef PROFILING
  if (RtsFlags.ProfFlags.doHeapProfile == HEAP_BY_RETAINER) {
    for (bd = firstStack; bd != NULL; bd = bd->link) 
      total_blocks += bd->blocks;
  }
#endif

  // count the blocks allocated by the arena allocator
  total_blocks += arenaBlocks();

  /* count the blocks on the free list */
  free_blocks = countFreeList();

  if (total_blocks + free_blocks != mblocks_allocated *
      BLOCKS_PER_MBLOCK) {
    fprintf(stderr, "Blocks: %ld live + %ld free  = %ld total (%ld around)\n",
	    total_blocks, free_blocks, total_blocks + free_blocks,
	    mblocks_allocated * BLOCKS_PER_MBLOCK);
  }

  ASSERT(total_blocks + free_blocks == mblocks_allocated * BLOCKS_PER_MBLOCK);
}


nat
countBlocks(bdescr *bd)
{
    nat n;
    for (n=0; bd != NULL; bd=bd->link) {
	n += bd->blocks;
    }
    return n;
}

/* Full heap sanity check. */
void
checkSanity( void )
{
    nat g, s;

    if (RtsFlags.GcFlags.generations == 1) {
	checkHeap(g0s0->to_blocks);
	checkChain(g0s0->large_objects);
    } else {
	
	for (g = 0; g < RtsFlags.GcFlags.generations; g++) {
	    for (s = 0; s < generations[g].n_steps; s++) {
		ASSERT(countBlocks(generations[g].steps[s].blocks)
		       == generations[g].steps[s].n_blocks);
		ASSERT(countBlocks(generations[g].steps[s].large_objects)
		       == generations[g].steps[s].n_large_blocks);
		if (g == 0 && s == 0) { continue; }
		checkHeap(generations[g].steps[s].blocks);
		checkChain(generations[g].steps[s].large_objects);
		if (g > 0) {
		    checkMutableList(generations[g].mut_list, g);
		    checkMutOnceList(generations[g].mut_once_list, g);
		}
	    }
	}
	checkFreeListSanity();
    }
}

// handy function for use in gdb, because Bdescr() is inlined.
extern bdescr *_bdescr( StgPtr p );

bdescr *
_bdescr( StgPtr p )
{
    return Bdescr(p);
}

#endif
