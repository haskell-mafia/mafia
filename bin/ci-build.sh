#!/bin/sh -exu

cabal update
cabal install --dependencies-only --enable-tests
cabal configure --enable-tests
cabal build

if [ "${USE_CACHE:-}" = "true" ]; then
  upload_cache
fi

dist/build/test/test
dist/build/test-io/test-io
dist/build/test-cli/test-cli
cabal haddock
if [ ${CABAL_VERSION} = "2.0" ]; then
  cabal sdist;
fi
