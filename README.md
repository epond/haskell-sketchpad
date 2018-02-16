# Haskell Sketchpad

My experiments with the Haskell language.

The intended use is to look at the source and unit tests.

## Build and run

`stack build` will build the module.

`stack exec haskell-sketchpad` will run it, but it probably won't do much.

`stack build --test` will run the tests.

`stack ghci` will fire up a repl with the project loaded so you can do things like check types and generally poke around.

Here is how you can run an individual test suite:
```
$ stack ghci haskell-sketchpad:spec
noise> hspec HaskellProgramming.Chapter15MonoidsSpec.spec
```

## Improvement Ideas

- Extract Haskell Programming book exercises to their own project