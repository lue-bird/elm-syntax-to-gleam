> Status: gleam does not seem to support record access type inference as good as elm.
Editor tooling knows the type better than the compiler.
> Also investigating why lazy module-level values are sometimes not used with ().

Print pure [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) declarations as
[gleam](https://gleam.run/) code.

```elm
import Elm.Parser
import ElmSyntaxToGleam

"""module Sample exposing (..)

plus2 : Int -> Int
plus2 n =
    n + ([ 2 ] |> List.sum)
"""
    |> Elm.Parser.parseToFile
    |> Result.mapError (\_ -> "failed to parse elm source code")
    |> Result.map
        (\syntaxModule ->
            [ syntaxModule ]
                |> ElmSyntaxToGleam.modules
                |> .declarations
                |> ElmSyntaxToGleam.gleamDeclarationsToModuleString
        )
-->
Ok """..some default declarations and imports..

pub fn sample_plus2() -> fn(Float) -> Float {
    fn(n) { float.add(n, float.sum([ 2.0 ])) }
}
"""
```

To try it out, you can
run [this node script](https://github.com/lue-bird/elm-syntax-to-gleam/tree/main/node-elm-to-gleam).

### be aware

-   only a subset of elm is currently supported. not supported:
    -   `elm/regex`, `elm/file`, `elm/bytes`, `elm/http`, `elm/random`, `elm/url`, `elm/json`, `elm/parser`, `elm/virtual-dom`,
        `elm/html`, `elm/svg`, `elm/browser`, `elm/time`, `elm-explorations/markdown`, `elm-explorations/webgl`, `elm-explorations/benchmark`, `elm-explorations/linear-algebra`
    -   `Platform`, `Platform.Cmd`, `Platform.Sub`, `Task`, `Process`
    -   **record update**, currying, ports, glsl, the prefix operator functions `(>>)` and `(<<)`
    -   `++` will default to `List.append` unless one of the arguments is a string literal. So e.g. use `a ++ b ++ ""` to append string variables (which is also faster in elm)
    - `List.minimum`, `List.maximum`, `List.sort`, `Basics.min`, `Basics.max` will only work for number lists.
    -   potential future candidates: `Basics.clamp`, `Basics.degrees`, `Basics.turns`,
        `Basics.radians`, `Basics.logBase`, `Basics.toPolar`, `Basics.fromPolar`, `Basics.never`, `Basics.sin`, `Basics.cos`, `Basics.tan`, `Basics.asin`, `Basics.acos`, `Basics.atan`, `Basics.atan2`, `Basics.e`, `Basics.pi`, `List.map5`, `List.map4`, `Char.toLocaleLower`, `Char.toLocaleUpper`, `Char.isAlpha`, `Char.isAlphaNum`, `Char.isDigit`, `Char.isOctDigit`, `Char.isHexDigit`, `List.map3/4/5`, `List.sortBy`, `List.head`, `List.tail` `Bitwise`, `Array`. Any help appreciated!
-   no checks are performed before transpiling to gleam. So if you don't add a compile check of your elm input,
    you might e.g. get a running program that circumvents an elm opaque type or phantom type, or a gleam program that can't be run
-   not much care has been put into making the resulting code readable or even conventionally formatted
    and comments are not preserved

Please [report any issues](https://github.com/lue-bird/elm-syntax-to-gleam/issues/new) you notice <3

### why gleam?
-   it's pretty much a superset of elm which makes transpiling easy
