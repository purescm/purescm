# purescm

## Testing

### Adding modules

1. Create the test cases in `test/resources/purescript/src/PureScheme/Test`.
   Keep each module as small as possible and don't be afraid to create multiple
   modules.

2. Run `stack run`.
   This will compile the modules to Scheme and leave the output in
   `test/resources/scheme`.

3. Check the output for correctness.

4. You're done, you just added new test cases.
   Commit both the source files and the output files.

### Moving/renaming/deleting modules

1. Make your changes to the PureScript source files.

2. Remove all `.sls` files in `test/resources/scheme`.

3. Run `stack run`.

4. Commit both the source files and the output files.
