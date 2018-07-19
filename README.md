mafia
=====

> Provides protection against cabal swindling, robbing, injuring or
> sabotaging people with chopsticks.

![mafia](img/mafia.jpg)


Overview
--------

Mafia is a light weight but opinionated wrapper around Cabal that makes working
on Haskell projects fun and easy.

The central idea is that upon cloning a project, you should only have to
run `mafia build` to get up and running for development. This will pull
down any git submodules, create a cabal sandbox, install dependencies
and then build everything, tests and benchmarks included.

Cabal packages in the same git repository, or in git submodules, are
discovered automatically and made available as source dependencies. This
is particularly useful if you have many internal dependencies that are
not published to Hackage. It allows for development of a package and its
dependencies all at once, without having to publish intermediate builds
of libraries and without having to resort to a monorepo.

All dependencies are cached globally in `$HOME/.mafia/packages`
using a nix-like hashing system so that for a given set of transitive
dependencies, a package is only ever built once. Source dependencies are
also cached, using their dependencies and source code as the hash. This
is critical for Haskell development at scale. At Ambiata we have around
150 Haskell packages which are being developed at a rapid pace, so it's
crucial that we don't need to think about curating blessed sets of
packages that we can cache as a single snapshot.


* [Using Mafia For Your Own Projects](doc/using-mafia.md).
* [Mafia Command Reference](doc/command-reference.md).


System configuration
--------------------

Mafia expects both GHC and Cabal to be installed and on the `PATH`.

Follow the guides below to configure your system correctly:

- [Installing GHC](doc/installing-ghc.md)
- [Installing Cabal](doc/installing-cabal.md)
