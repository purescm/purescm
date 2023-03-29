module PureScript.Backend.Chez.Constants where

import Prelude

libChezSchemePrefix :: String
libChezSchemePrefix = "scm:"

runtimePrefix :: String
runtimePrefix = "rt:"

scmPrefixed :: String -> String
scmPrefixed = append libChezSchemePrefix

rtPrefixed :: String -> String
rtPrefixed = append runtimePrefix

moduleForeign :: String
moduleForeign = "foreign"

moduleLib :: String
moduleLib = "lib"
