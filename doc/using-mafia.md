Using Mafia for your own Projects
=================================

Mafia is designed to be run via a shell script which will bootstrap the
`mafia` binary, but you can quite happily put the `mafia` binary on your
`PATH` and use it like that as well.

Place the
[script](https://raw.githubusercontent.com/ambiata/mafia/master/script/mafia)
in the same directory as your `.cabal` package file and then run
`./mafia upgrade`. This will ensure that you have the latest script and
also bake the git hash of the Mafia version in to the script.

```
curl -O https://raw.githubusercontent.com/ambiata/mafia/master/script/mafia && chmod +x mafia
```

You can now run `./mafia build` for the first time. This will build the
Mafia executable with the git hash specified in the script, and then
build the current package. Having the hash baked in to the script means
that anyone trying to build the package will be using exactly the same
version of Mafia.

To upgrade to the latest Mafia, you can `./mafia upgrade` again at any time.

Mafia executables are stored in `$HOME/.mafia/bin`.
