# purs-backend-chez

## Snapshots

To run snapshots, run `npm run test`.
To overwrite old snapshots with the latest output, run `npm run test -- -a "--accept"`.
To add a new snapshot, create a file called `Snapshot.X.purs` where `X` indicates what is being tested.
If a snapshot needs a dependency, install it by running `spago -x test-snapshots/spago.dhall install <packages...>`. The snapshots have their own `spago.dhall` file so as not to pollute the `purs-backend-chez` binary with unneeded dependencies.
