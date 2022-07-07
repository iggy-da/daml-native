{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DamlSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Prelude (module X, module Prelude) where
import GHC.Types as X
import "base" Prelude as X
import "ghc-prim" GHC.Types

import "ghc-prim" GHC.Types

primitive : forall (f : Symbol) b. b
primitive = primitive


import Data.String (IsString(..))

data Any
data ContractId a
data Update a
data TypeRep
data Party
data Text

data Optional a = None | Some a

data Consuming t = Consuming {}

data Archive = Archive {}

instance IsString Text where fromString = undefined

class IsParties a where
  toParties : a -> [Party]

instance IsParties Party where
  toParties p = [p]

instance IsParties [Party] where
  toParties ps = ps

instance IsParties (Optional Party) where
  toParties None = []
  toParties (Some p) = [p]

instance Eq Party where (==) = undefined
instance Show Party where show = undefined

instance Functor Update where
    fmap f x = x >>= \v -> pure (f v)

instance Applicative Update where
    pure = undefined
    f <*> x = f >>= \f -> x >>= \x -> pure (f x)

instance Monad Update where
    (>>=) = undefined

class HasSignatory t where
  signatory : t -> [Party]

class HasObserver t where
  observer : t -> [Party]

class HasEnsure t where
  ensure : t -> Bool

class HasAgreement t where
  agreement : t -> Text

class HasCreate t where
  create : t -> Update (ContractId t)

class HasFetch t where
  fetch : ContractId t -> Update t

class HasArchive t where
  archive : ContractId t -> Update ()

class HasTemplateTypeRep t where
  _templateTypeRep : proxy t -> TypeRep

class HasToAnyTemplate t where
  _toAnyTemplate : t -> Any

class HasFromAnyTemplate t where
  _fromAnyTemplate : Any -> Optional t

class HasExercise t c r | t c -> r where
  exercise : ContractId t -> c -> Update r

class HasToAnyChoice t c r | t c -> r where
  _toAnyChoice : proxy t -> c -> Any

class HasFromAnyChoice t c r | t c -> r where
  _fromAnyChoice : proxy t -> Any -> Optional c

class HasController t c where
  getController : t -> c -> [Party]

class HasChoiceApply r t c where
  applyChoice : ContractId t -> t -> c -> Update r

class HasConsuming t c where
  consuming : Consuming t

class HasChoiceObserver t c where
  choiceObserver : Optional (t -> c -> [Party])

