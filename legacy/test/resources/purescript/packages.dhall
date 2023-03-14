let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20210826/packages.dhall sha256:eee0765aa98e0da8fc414768870ad588e7cada060f9f7c23c37385c169f74d9f
in  upstream
    with prelude.repo = "https://github.com/purescm/purescript-prelude.git"
    with prelude.version = "5c07bfe1b1e335cec567b56137a9c9f14cf4b039"

    with effect.repo = "https://github.com/purescm/purescript-effect.git"
    with effect.version = "cbb6c88b6e2b673e9a6d703b5428b0ed86fc491e"

    with console.repo = "https://github.com/purescm/purescript-console.git"
    with console.version = "f215dd11dc7ab8ef3b4e1e62c1bffcc5d6e60d6a"

    with numbers.repo = "https://github.com/purescm/purescript-numbers"
    with numbers.version = "9c56b14f648215c430d9d0a51eab88fc972f726f"

    with integers.repo = "https://github.com/purescm/purescript-integers"
    with integers.version = "001d905c61f52156f856b86ccace00c6411adfb3"

    with functions.repo = "https://github.com/purescm/purescript-functions"
    with functions.version = "d46a4cbb791938b2708210d693de1cf79c84d82c"

    with math.repo = "https://github.com/purescm/purescript-math"
    with math.version = "ca667bed6dc4041afbc8378a1fc8d8ece6f58786"

    with refs.repo = "https://github.com/purescm/purescript-refs"
    with refs.version = "479d202dcd1be34f7b2ce1dec2c0a1cac5b620d9"

    with partial.repo = "https://github.com/purescm/purescript-partial"
    with partial.version = "55dcaff84b53a5c514dfae85312d663842f24842"

    with control.repo = "https://github.com/purescm/purescript-control"
    with control.version = "bd4cc0d654999bca7f28ca31e9b34c3e85e0e889"

    with unsafe-coerce.repo = "https://github.com/purescm/purescript-unsafe-coerce"
    with unsafe-coerce.version = "8e077884b7843892ddd889ad50367452fa397423"

    with unfoldable.repo = "https://github.com/purescm/purescript-unfoldable"
    with unfoldable.version = "d36166652dfdce3f55e93aecb21657dafb66e398"

    with foldable-traversable.repo = "https://github.com/purescm/purescript-foldable-traversable"
    with foldable-traversable.version = "61db781115f7f8e1c4cab46d619022eb99c3df06"

    with minibench.repo = "https://github.com/purescm/purescript-minibench"
    with minibench.version = "36c106da2287f52653eaff9d2e3670285c1d41c9"
