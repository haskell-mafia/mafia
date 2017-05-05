Mafia Command Reference
=======================

Many of Mafia's commands are, more or less, direct wrappers of Cabal
commands. The main difference is that they have a pre-step which takes
care of installing any dependencies your package has, so you never have
to think about it.

### mafia build

Builds a package, including all executables, test suites and benchmarks.

Mafia uses whichever version of
[GHC](doc/installing-ghc.md) and
[Cabal](doc/installing-cabal.md) it
finds on the `PATH` to build a project. It keeps cabal sandboxes in
version specific directories, so that it is easy to
[switch](doc/installing-ghc.md#advanced-multiple-ghc-versions)
between versions of GHC while developing a package.

Using the switching setup linked above, a typical multi-GHC session
might look as follows:

```
$ g
The Glorious Glasgow Haskell Compilation System, version 7.8.4
$ mafia build
Writing a default package environment file to
/Users/bob/src/foo/cabal.sandbox.config
Creating a new sandbox at
/Users/bob/src/foo/.cabal-sandbox/x86_64-apple-darwin/7.8.4
Checking for changes to dependencies...
Installing dependencies...
-- snip --
$ g
The Glorious Glasgow Haskell Compilation System, version 7.10.2
$ mafia build
Writing a default package environment file to
/Users/bob/src/foo/cabal.sandbox.config
Creating a new sandbox at
/Users/bob/src/foo/.cabal-sandbox/x86_64-apple-darwin/7.10.2
Checking for changes to dependencies...
Installing dependencies...
-- snip --
```

Mafia always works in conjunction with cabal and tries to stay as
compatible as possible. This means that after a `mafia build` it should
be possible to run cabal commands (such as `cabal exec`) and have cabal
benefit from the sandbox which Mafia has constructed.

Mafia makes it easy to build your executable with profiling enabled,
just run `mafia build -p`. Mafia will then build profiling versions of
all the packages which your package depends on. Packages in the global
cache are upgraded to support profiling in-place, so you don't need to
perform the regular build for those packages all over again.


### mafia clean

Clean up after a build. This removes the Cabal sandbox and the `dist` directory.


### mafia test

Test this package, by default this runs all test suites.


### mafia bench

Benchmark this package, by default this runs all the benchmarks.


### mafia lock

Creates a lock file, which pins the versions of the package's transitive
dependencies to specific version numbers.


### mafia unlock

Deletes the lock file, allowing the Cabal solver to choose new versions
of packages again.


### mafia repl

Start GHCi via `cabal repl`, by default on the library of the package.


### mafia quick

Start GHCi directly on one or more files.

This is particularly convenient when used with the `--all` (or `-a`)
option. This loads all the source dependencies of the specified files in
GHCi as well, so you can modify a dependency and simply reload with
`:r`, no building required.

```
~/src/mismi/mismi-ec2 $ mafia quick -a src/**/*.hs
Checking for changes to dependencies...
GHCi, version 7.8.4: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
[ 1 of 20] Compiling Mismi.EC2.Amazonka ( src/Mismi/EC2/Amazonka.hs, interpreted )
[ 2 of 20] Compiling Mismi.Amazonka   ( ../mismi-core/src/Mismi/Amazonka.hs, interpreted )
[ 3 of 20] Compiling P.Debug          ( ../lib/p/src/P/Debug.hs, interpreted )
[ 4 of 20] Compiling P.Foldable       ( ../lib/p/src/P/Foldable.hs, interpreted )
[ 5 of 20] Compiling P.Function       ( ../lib/p/src/P/Function.hs, interpreted )
[ 6 of 20] Compiling P.List           ( ../lib/p/src/P/List.hs, interpreted )
[ 7 of 20] Compiling P.Ord            ( ../lib/p/src/P/Ord.hs, interpreted )
[ 8 of 20] Compiling P.Monoid         ( ../lib/p/src/P/Monoid.hs, interpreted )
[ 9 of 20] Compiling P.Maybe          ( ../lib/p/src/P/Maybe.hs, interpreted )
[10 of 20] Compiling P.Either         ( ../lib/p/src/P/Either.hs, interpreted )
[11 of 20] Compiling P.Bool           ( ../lib/p/src/P/Bool.hs, interpreted )
[12 of 20] Compiling P.Applicative    ( ../lib/p/src/P/Applicative.hs, interpreted )
[13 of 20] Compiling P                ( ../lib/p/src/P.hs, interpreted )
[14 of 20] Compiling Mismi.EC2.Data   ( src/Mismi/EC2/Data.hs, interpreted )
[15 of 20] Compiling Mismi.EC2.Commands ( src/Mismi/EC2/Commands.hs, interpreted )
[16 of 20] Compiling Mismi.EC2.Config.Virtualization ( src/Mismi/EC2/Config/Virtualization.hs, interpreted )
[17 of 20] Compiling X.Control.Monad.Catch ( ../lib/x/x-exception/src/X/Control/Monad/Catch.hs, interpreted )
[18 of 20] Compiling X.Control.Monad.Trans.Either ( ../lib/x/x-eithert/src/X/Control/Monad/Trans/Either.hs, interpreted )
[19 of 20] Compiling Mismi.EC2.Metadata ( src/Mismi/EC2/Metadata.hs, interpreted )
[20 of 20] Compiling Mismi.EC2        ( src/Mismi/EC2.hs, interpreted )
Ok, modules loaded: X.Control.Monad.Catch, X.Control.Monad.Trans.Either, P, P.Applicative, P.Bool, P.Either, P.Maybe, P.Monoid, P.Ord, P.List, P.Function, P.Foldable, P.Debug, Mismi.Amazonka, Mismi.EC2, Mismi.EC2.Commands, Mismi.EC2.Config.Virtualization, Mismi.EC2.Data, Mismi.EC2.Metadata, Mismi.EC2.Amazonka.
*Mismi.EC2
λ _
```


### mafia watch

Watches the filesystem for changes and stays running, compiles and gives
quick feedback.

Similar to `mafia quick` it needs a list of files to load. To use it for
running tests you can pass `-T EXPR`:

```
$ mafia watch test/test.hs -- -T Test.Pure.tests
```

This command is powered by [ghcid](https://github.com/ndmitchell/ghcid),
and will automatically install it to `$HOME/.mafia/bin` on first
use.


### mafia hoogle

Searches all the Hackage packages you have in your global cache with
a Hoogle query.

For example:

```
$ mafia hoogle -- --count=5 'a -> Maybe a -> a'
Data.Maybe fromMaybe :: a -> Maybe a -> a
Data.Maybe fromMaybe :: a -> Maybe a -> a
Maybe fromMaybe :: a -> Maybe a -> a
Control.Conditional (<|) :: a -> Maybe a -> a
Safe fromJustDef :: a -> Maybe a -> a
```

It can be useful to have this available from inside GHCi, to do this,
add the following to you `~/.ghc/ghci.conf` file:

```
:def hoogle \str -> return $ ":!mafia hoogle -- --count=15 \"" ++ str ++ "\""
```

Then from inside GHCi, you can use it as follows:

```
λ :hoogle a -> [a]
Prelude repeat :: a -> [a]
Data.List repeat :: a -> [a]
Prelude repeat :: a -> [a]
Data.List repeat :: a -> [a]
Prelude repeat :: a -> [a]
List repeat :: a -> [a]
Test.QuickCheck.Arbitrary shrinkNothing :: a -> [a]
Test.QuickCheck shrinkNothing :: a -> [a]
Prelude iterate :: (a -> a) -> a -> [a]
Data.List iterate :: (a -> a) -> a -> [a]
Prelude iterate :: (a -> a) -> a -> [a]
Data.List iterate :: (a -> a) -> a -> [a]
Prelude iterate :: (a -> a) -> a -> [a]
List iterate :: (a -> a) -> a -> [a]
Data.List genericReplicate :: Integral i => i -> a -> [a]
```

This command is powered by
[hoogle](https://github.com/ndmitchell/hoogle) and will install it to
`$HOME/.mafia/bin` on first use.


### mafia depends

Show the transitive dependencies of this package, by default displays as
a list.

Running this command on Mafia itself looks something like this:

```
~/src/mafia $ mafia depends
aeson-0.8.0.2 +old-locale
ambiata-disorder-core-0.0.1-4d73a2fdb30c255434d37cbed9d7dee8feaa7a7e
ambiata-disorder-corpus-0.0.1-f5ef0e865e27b950e3f76e9bcdf172d0b89450da
ambiata-p-0.0.1-5ac1fbfc47e28490db66ea959fc9bddc55456d00
ambiata-twine-0.0.1-caf2e76454b20fcb0e001d03d50080b0b9153c60
ambiata-x-eithert-0.0.1-2be3c283eedb551496c2796ce1221eb7e4464758
ambiata-x-exception-0.0.1-e2e84f18475f307b668261c1af0d024044fd3488
ambiata-x-optparse-0.0.1-b4dbdb36ba29497a29ff82251c9e2811efc32f49
ansi-terminal-0.6.2.3
ansi-wl-pprint-0.6.7.3
async-2.0.2
attoparsec-0.12.1.6
base16-bytestring-0.1.1.6
bifunctors-5
byteable-0.1.1
case-insensitive-1.2.0.5
-- snip --
```

Dependencies can also be rendered as a tree:

```
~/src/mafia $ mafia depends --tree
aeson-0.8.0.2 +old-locale
 ├─╼ attoparsec-0.12.1.6
 │    ├─╼ scientific-0.3.4.4
 │    │    ├─╼ hashable-1.2.4.0
 │    │    │    └─╼ text-1.2.2.0
 │    │    ├─╼ text-1.2.2.0
 │    │    └─╼ vector-0.11.0.0
 │    │         └─╼ primitive-0.6.1.0
 │    │              └─╼ transformers-0.4.3.0
 │    └─╼ text-1.2.2.0
 ├─╼ dlist-0.7.1.2
 ├─╼ hashable-1.2.4.0
 │    └─╼ text-1.2.2.0
-- snip --
```

Finally, it can be used to discover why you're pulling in a particular package:

```
~/src/mafia $ mafia depends --tree scientific
aeson-0.8.0.2 +old-locale
 ├─╼ attoparsec-0.12.1.6
 │    └─╼ scientific-0.3.4.4
 └─╼ scientific-0.3.4.4
ambiata-x-optparse-0.0.1-b4dbdb36ba29497a29ff82251c9e2811efc32f49
 └─╼ attoparsec-0.12.1.6
      └─╼ scientific-0.3.4.4
attoparsec-0.12.1.6
 └─╼ scientific-0.3.4.4
quickcheck-instances-0.3.12
 └─╼ scientific-0.3.4.4
```


### mafia hash

Hash the contents of this package. This is useful for checking
that a `.mafiaignore` file is working correctly. The hash denoted by
`(package)` in this command's output is the one used by Mafia to track
changes to source dependencies.

```
~/src/mafia $ mafia hash
2b8b815229aa8a61e483fb4ba0588b8b6c491890  LICENSE
0cb64bd71ab2c6da2778d08fcb59bed3a8bc37ec  Setup.hs
-- snip --
1c26770ac9959702def26e7329b2fc3717f5a24d  test/test-io.hs
6641c2ffd7cf01706ce7ee2052472db4e8f19fdb  test/test.hs
f94246d177847a0c17de2ab076ca08c25e295fd7  (file list)
e71cc3b72789608dc0afb947022f2c3a1a75510c  (package)
```

If you plan to use a package as a source dependency, it is a good idea
to verify that the package's hash is the same every time you run `mafia
hash`. Sometimes you can have a situation where a custom `Setup.hs` will
generate files with timestamps in them. It is best to add the paths to
these generated files to your package's `.mafiaignore`, so that they are
not considered when calculating the hash. Without this, Mafia will
rebuild and reinstall the package on every build.


### mafia install

Install a Hackage package and print the path to its `bin/` directory to `stdout`.

```
$ mafia install pretty-show
stderr> Building...
stderr> Building...
stdout> /Users/bob/.ambiata/mafia/bin/pretty-show/bin
```

Receiving the path on `stdout` means that it can be used in the following way:

```
$ echo "Foo(Bar     (Baz 1 2    3)   )" | $(mafia install pretty-show)/ppsh
Foo (Bar (Baz 1 2 3))
```


### mafia update

The same as `cabal update`, but limited to retrieving a new index
tarball at most once per day. This is useful for scripting CI build bots
to prevent them from wasting time updating from Hackage when there are
likely no interesting changes.


### mafia exec

Exec a command in the current cabal sandbox. This can be useful for compiling
a Haskell source file that is not in the cabal file but with full access to
packages installed in the local sandbox:

```
mafia exec -- ghc -Wall wibble.hs -o wibble
```


System configuration
--------------------

Mafia expects both GHC and Cabal to be installed and on the `PATH`.

Follow the guides below to configure your system correctly:

- [Installing GHC](doc/installing-ghc.md)
- [Installing Cabal](doc/installing-cabal.md)
