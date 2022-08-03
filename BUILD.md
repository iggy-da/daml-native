```
git submodule update --init --recursive
docker run -u1000 --rm -i -t -v `pwd`:/home/ghc gregweber/ghc-haskell-dev /bin/bash
cd /home/ghc
./boot
cabal update
#cabal install cabal-install
./hadrian/build.cabal.sh --flavour=devel2
```
