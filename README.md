# haskell-to-elm-test

End-to-end fuzz testing for:

* [elm-syntax](https://github.com/haskell-to-elm/elm-syntax)
* [haskell-to-elm](https://github.com/haskell-to-elm/haskell-to-elm)
* [servant-to-elm](https://github.com/haskell-to-elm/servant-to-elm)

# Usage

To run the tests, first execute

```
make run-backend
```

and then, in a separate terminal, run

```
make run-frontend
```

which opens a browser that runs the tests.
