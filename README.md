# purescm

Chez Scheme backend for the PureScript programming language.

## Development

The recommended way to get started with development is to install the [Nix](https://nixos.org/download.html) package manager, and then invoking `nix-shell` in order to get a development environment with the project's dependencies installed. Additionally, this repository also supports [Flakes](https://nixos.wiki/wiki/Flakes), allowing you to run `nix develop` instead. `nix-direnv` is also a convenient tool to have.
```sh
dev@nix:~/code/purescm$ nix-shell

[nix-shell:~/code/purescm]$ which purs
/nix/store/h5wa7f8jqrn3qssir7zjys11q4nwkgck-purescript-v0.15.8/bin/purs

[nix-shell:~/code/purescm]$ which spago
/nix/store/ljya6ih1l375kkpg43achs6wac0ng70q-spago-0.93.1/bin/spago
```

## SRFIs

`purescm` depends on [chez-srfi](https://github.com/arcfide/chez-srfi) as a runtime dependency, and is vendored under the `vendor` directory. To update `chez-srfi`, edit the revision defined in `tools/src/Main.purs`, run `npm run srfi`, and commit the resulting changes if there are any.

## Tests

To perform snapshot tests, run `npm run test`.

To overwrite old snapshots with the latest output, run `npm run test-accept`.

To add a new snapshot, create a file called `Snapshot.X.purs` where `X` indicates what is being tested.

If a snapshot needs a dependency, install it by editing the `test-snapshots/spago.yaml` file. If the dependency has FFI written in JavaScript, this would have to be ported to Scheme code as well. See existing forks of packages in the aforementioned `spago.yaml` file for reference.
