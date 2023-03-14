// A majority of the code below was copied from 
// https://github.com/aristanetworks/purescript-backend-optimizer/blob/main/backend-es/test/Utils.js
//
// To fullfill copyright requirements...
//    Copyright © 2022 Arista Networks, Inc.
//    MIT license: https://opensource.org/license/mit/
export const loadModuleMainImpl = onError => onSuccessMain => onSuccess => path => () => {
  import(path).then(
    mod => mod.main ? onSuccessMain(mod.main)() : onSuccess(),
    err => onError(err)()
  );
}
