Installing Cabal
----------------

Cabal comes in two parts, the `cabal` executable (which is in the
cabal-install package) and the Cabal library (which is in the Cabal
package).

#### Install cabal-install 1.24.0.0

These instructions copy the `cabal` executable to `$HOME/bin` which is
assumed to be on your `$PATH`. If you have an alternative location where
you prefer to put executables, then use that instead.

```sh
$ wget http://hackage.haskell.org/package/cabal-install-1.24.0.0/cabal-install-1.24.0.0.tar.gz
$ tar xf cabal-install-1.24.0.0.tar.gz
$ cd cabal-install-1.24.0.0
$ EXTRA_CONFIGURE_OPTS="" ./bootstrap.sh --sandbox --no-doc
# $HOME/bin is assumed to exist and be on your $PATH
$ cp .cabal-sandbox/bin/cabal $HOME/bin/cabal
```

#### Install Cabal 1.24.0.0

*If you followed the advanced instructions
[here](https://github.com/ambiata/mafia/blob/master/doc/ghc.md#advanced-multiple-ghc-versions)
for installing multiple GHC versions, then you will need to repeat this
step for each of the GHC versions that you have installed.*

In order for custom `Setup.hs` files to operate correctly, cabal-install
needs the Cabal library it was built against available in the user (or
the global) package database.

If you installed a different cabal-install than the one above, or you
didn't use the `bootstrap.sh` script, then make sure you run `cabal
--version` to double check the version of the Cabal library that you
need.

*GHC 8.0.1 ships with Cabal-1.24.0.0 out of the box, so you may already
have it installed. In any case, there is no harm in performing this step
redundantly.*

```sh
# double-check that we're installing the right version of the Cabal library
$ cabal --version
cabal-install version 1.24.0.0
using version 1.24.0.0 of the Cabal library
# make sure we're not in a sandbox, jumping to $HOME is a safe bet
$ cd
$ cabal install Cabal-1.24.0.0
```

If you need to install a newer Cabal library, you don't need to remove
the old one, they should live side-by-side happily.

### Locking down your user package database (optional)

If you want to make sure that nothing messes with your user package
database, it can be helpful to set it to read-only. Just remember to
make it writable if you ever need to install a newer Cabal library.

```sh
$ chmod -R -w $HOME/.ghc/x86_64-darwin-<GHC_VERSION>/package.conf.d
```

To make it writable again:

```sh
$ chmod -R +w $HOME/.ghc/x86_64-darwin-<GHC_VERSION>/package.conf.d
```

### Troubleshooting / Next Steps

When running `./mafia quick` or `./mafia watch`, you _may_ see something like the following from a fresh checkout:

```
Arbitrary.hs:55:11:
    No instance for (Arbitrary time-1.4.2:Data.Time.Clock.UTC.UTCTime)
```

Run `ghc-pkg list`:

```
/usr/local/lib/ghc-7.8.4/package.conf.d
   Cabal-1.18.1.5
  ...
/Users/<USER>/.ghc/x86_64-darwin-7.8.4/package.conf.d
   Cabal-1.22.3.0
   ...
```

If there are extra packages in the `.ghc` packages directory, _other_ than `Cabal-*`, then unregister them:

```
$ ghc-pkg list --user | grep -v 'package.conf.d' | grep -v 'Cabal-' | xargs ghc-pkg unregister
```
