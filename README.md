# purescm - Chez Scheme backend for PureScript

## Installation

```
npm i --global purescm@next
```

## Usage

The best way to use `purescm` is to use the [spago](https://github.com/purescript/spago) integration for alternative PureScript backends. In `spago.yaml` use the `backend` config like so:

```
workspace:
  backend:
    cmd: purescm
    args:
      - "build"
```

Then to compile your `purescm` project you can run:

```
spago build
```

This will produce output under `output/`. You can then further precompile the scheme files to a single Chez program:

```
purescm bundle-app --main Main
```

which generates a single file `output/main`. To run the compiled program:

```
scheme --program output/main
```

## Vendored Dependencies

In order to represent `Array` and record types in `purescm`, the backend has a runtime dependency on implementations of SRFI 214 (Flexvectors) and SRFI 125 (Intermediate hash tables). This is due in part to how [vectors](https://cisco.github.io/ChezScheme/csug9.5/objects.html#./objects:h5) are more like "arrays" than "array lists" and [hashtables](https://cisco.github.io/ChezScheme/csug9.5/objects.html#./objects:h12)' canonical accessor, `hashtable-ref`, takes a `default` value rather than raising an exception.

We vendor [chez-srfi](https://github.com/arcfide/chez-srfi) using the following steps:

1. Clone `chez-srfi` at the project root.

2. Navigate to `chez-srfi` and run `./install.chezscheme.sps ../vendor`. This should produce an `srfi` folder inside of `vendor`.

3. Copy the desired SRFIs from `vendor/srfi` into `vendor/purs/runtime/srfi`. Make sure to also copy the SRFIs and other scheme files (e.g. the ones in the `private` folder) that they depend on. The [library-requirements](https://cisco.github.io/ChezScheme/csug9.5/libraries.html#./libraries:h7) function can aid in this process.

4. Modify the copied scheme files such that the library names align with the folder structure. [`sd`](https://github.com/chmln/sd) makes this easy!
```sh
sd "\(srfi " "(purs runtime srfi " vendor/purs/**/*.sls
```

5. To verify, simply invoke the Scheme REPL:
```scheme
$ scheme --libdirs ./vendor:
Chez Scheme Version 9.5.8
Copyright 1984-2022 Cisco Systems, Inc.

> (import (purs runtime srfi :125))
>
```

## Snapshots

To run snapshots, run `npm run test`.
To overwrite old snapshots with the latest output, run `npm run test -- -a "--accept"`.
To add a new snapshot, create a file called `Snapshot.X.purs` where `X` indicates what is being tested.
If a snapshot needs a dependency, install it by running `spago -x test-snapshots/spago.dhall install <packages...>`. The snapshots have their own `spago.dhall` file so as not to pollute the `purescm` binary with unneeded dependencies.
