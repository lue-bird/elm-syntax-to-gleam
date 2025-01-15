module ElmSyntaxToGleam exposing
    ( modules, gleamDeclarationsToModuleString
    , GleamLetDeclaration(..), GleamExpression(..), GleamPattern(..), GleamType(..)
    )

{-| Transpiling [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/)
declarations to gleam.

@docs modules, gleamDeclarationsToModuleString
@docs GleamLetDeclaration, GleamExpression, GleamPattern, GleamType

If you need more fine-gleamed helpers,
[open an issue](https://github.com/lue-bird/elm-syntax-format/issues/new)

-}

import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Import
import Elm.Syntax.Infix
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation
import FastDict
import FastSet
import Print exposing (Print)
import Unicode


{-| The sub-set of F# type syntax used in generated code
-}
type GleamType
    = GleamTypeConstruct
        { moduleOrigin : Maybe String
        , name : String
        , arguments : List GleamType
        }
    | GleamTypeTuple
        { part0 : GleamType
        , part1 : GleamType
        , part2Up : List GleamType
        }
    | GleamTypeVariable String
    | GleamTypeFunction
        { input : List GleamType
        , output : GleamType
        }


{-| The sub-set of F# pattern syntax used in generated code
-}
type GleamPattern
    = GleamPatternIgnore
    | GleamPatternNumber Float
    | GleamPatternChar Char
    | GleamPatternString String
    | GleamPatternVariable String
    | GleamPatternAs
        { variable : String
        , pattern : GleamPattern
        }
    | GleamPatternListCons
        { initialElement0 : GleamPattern
        , initialElement1Up : List GleamPattern
        , tail : Maybe String
        }
    | GleamPatternListExact (List GleamPattern)
    | GleamPatternVariant
        { moduleOrigin : Maybe String
        , name : String
        , values : List GleamPattern
        }
    | GleamPatternTuple
        { part0 : GleamPattern
        , part1 : GleamPattern
        , part2Up : List GleamPattern
        }


{-| The sub-set of F# expression syntax used in generated code
-}
type GleamExpression
    = GleamExpressionFloat Float
    | GleamExpressionChar Char
    | GleamExpressionString String
    | GleamExpressionReference
        { moduleOrigin : Maybe String
        , name : String
        }
    | -- because gleam does not support declaring most module-level values as const
      GleamExpressionReferenceModuleLevelValueAsLazyFn
        { moduleOrigin : Maybe String
        , name : String
        }
    | GleamExpressionRecordAccess
        { record : GleamExpression
        , field : String
        }
    | GleamExpressionTuple
        { part0 : GleamExpression
        , part1 : GleamExpression
        , part2Up : List GleamExpression
        }
    | GleamExpressionList (List GleamExpression)
    | GleamExpressionRecord (FastDict.Dict String GleamExpression)
    | GleamExpressionCall
        { called : GleamExpression
        , argument0 : GleamExpression
        , argument1Up : List GleamExpression
        }
    | GleamExpressionLambda
        { parameter0 : Maybe String
        , parameter1Up : List (Maybe String)
        , result : GleamExpression
        }
    | GleamExpressionCase
        { matched : GleamExpression
        , case0 :
            { pattern : GleamPattern
            , result : GleamExpression
            }
        , case1Up :
            List
                { pattern : GleamPattern
                , result : GleamExpression
                }
        }
    | GleamExpressionWithLetDeclarations
        { declaration0 : GleamLetDeclaration
        , declaration1Up : List GleamLetDeclaration
        , result : GleamExpression
        }


{-| The sub-set of gleam local declaration syntax used in generated gleam code
-}
type GleamLetDeclaration
    = GleamLetDestructuring
        { pattern : GleamPattern
        , expression : GleamExpression
        }
    | GleamLetDeclarationValueOrFunction
        { name : String
        , result : GleamExpression
        , type_ : Maybe GleamType
        }


{-| How do references used in a module map to their origin module?

Contains variants, variant function and value declaration names.

-}
type alias ModuleContext =
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    }


{-| Calculate valid mappings of qualifications + name
to origin module based on a module's imports.

Requires all exposed names
so we can resolve `exposing (..)` and `EnumType(..)`.

-}
importsToModuleContext :
    FastDict.Dict
        Elm.Syntax.ModuleName.ModuleName
        { valueOrFunctionOrTypeAliasNames : FastSet.Set String
        , enumTypesExposingVariants :
            FastDict.Dict String (FastDict.Dict String { valueCount : Int })
        }
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Import.Import)
    -> ModuleContext
importsToModuleContext moduleExposes imports =
    let
        importsNormal :
            List
                { moduleName : Elm.Syntax.ModuleName.ModuleName
                , alias : Maybe String
                , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
                    FastSet.Set String
                , exposedVariants :
                    FastDict.Dict String { valueCount : Int }
                }
        importsNormal =
            implicitImports
                ++ (imports
                        |> List.map
                            (\(Elm.Syntax.Node.Node _ syntaxImport) ->
                                let
                                    importModuleName : Elm.Syntax.ModuleName.ModuleName
                                    importModuleName =
                                        syntaxImport.moduleName |> Elm.Syntax.Node.value

                                    exposes :
                                        { valuesAndFunctionsAndTypeAliasesAndEnumTypes :
                                            FastSet.Set String
                                        , variants :
                                            FastDict.Dict String { valueCount : Int }
                                        }
                                    exposes =
                                        case syntaxImport.exposingList of
                                            Nothing ->
                                                { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                    FastSet.empty
                                                , variants = FastDict.empty
                                                }

                                            Just (Elm.Syntax.Node.Node _ syntaxExposing) ->
                                                case moduleExposes |> FastDict.get importModuleName of
                                                    Nothing ->
                                                        { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                            FastSet.empty
                                                        , variants = FastDict.empty
                                                        }

                                                    Just moduleExposedNames ->
                                                        case syntaxExposing of
                                                            Elm.Syntax.Exposing.All _ ->
                                                                { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                                    moduleExposedNames.enumTypesExposingVariants
                                                                        |> FastDict.foldl
                                                                            (\enumTypeName _ soFar ->
                                                                                soFar |> FastSet.insert enumTypeName
                                                                            )
                                                                            moduleExposedNames.valueOrFunctionOrTypeAliasNames
                                                                , variants =
                                                                    moduleExposedNames.enumTypesExposingVariants
                                                                        |> FastDict.foldl
                                                                            (\_ variantNames soFar -> FastDict.union variantNames soFar)
                                                                            FastDict.empty
                                                                }

                                                            Elm.Syntax.Exposing.Explicit explicitEposes ->
                                                                explicitEposes
                                                                    |> List.foldl
                                                                        (\(Elm.Syntax.Node.Node _ expose) soFar ->
                                                                            case expose of
                                                                                Elm.Syntax.Exposing.InfixExpose _ ->
                                                                                    soFar

                                                                                Elm.Syntax.Exposing.TypeOrAliasExpose name ->
                                                                                    { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                                                        soFar.valuesAndFunctionsAndTypeAliasesAndEnumTypes
                                                                                            |> FastSet.insert name
                                                                                    , variants = soFar.variants
                                                                                    }

                                                                                Elm.Syntax.Exposing.FunctionExpose name ->
                                                                                    { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                                                        soFar.valuesAndFunctionsAndTypeAliasesAndEnumTypes
                                                                                            |> FastSet.insert name
                                                                                    , variants = soFar.variants
                                                                                    }

                                                                                Elm.Syntax.Exposing.TypeExpose enumTypeExpose ->
                                                                                    { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                                                        soFar.valuesAndFunctionsAndTypeAliasesAndEnumTypes
                                                                                            |> FastSet.insert enumTypeExpose.name
                                                                                    , variants =
                                                                                        case enumTypeExpose.open of
                                                                                            Nothing ->
                                                                                                soFar.variants

                                                                                            Just _ ->
                                                                                                case
                                                                                                    moduleExposedNames.enumTypesExposingVariants
                                                                                                        |> FastDict.get enumTypeExpose.name
                                                                                                of
                                                                                                    Nothing ->
                                                                                                        soFar.variants

                                                                                                    Just enumTypeDeclared ->
                                                                                                        FastDict.union
                                                                                                            soFar.variants
                                                                                                            enumTypeDeclared
                                                                                    }
                                                                        )
                                                                        { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                                                            FastSet.empty
                                                                        , variants = FastDict.empty
                                                                        }
                                in
                                { moduleName = importModuleName
                                , alias =
                                    syntaxImport.moduleAlias
                                        |> Maybe.map
                                            (\(Elm.Syntax.Node.Node _ syntaxAlias) ->
                                                syntaxAlias |> String.join "."
                                            )
                                , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                    exposes.valuesAndFunctionsAndTypeAliasesAndEnumTypes
                                , exposedVariants =
                                    exposes.variants
                                }
                            )
                   )
                |> importsCombine
    in
    importsNormal
        |> List.foldl
            (\syntaxImport soFar ->
                let
                    importedModuleMembers :
                        { valuesAndFunctionsAndTypeAliasesAndEnumTypes :
                            FastSet.Set String
                        , variants : FastDict.Dict String { valueCount : Int }
                        }
                    importedModuleMembers =
                        case moduleExposes |> FastDict.get syntaxImport.moduleName of
                            Nothing ->
                                { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                    FastSet.empty
                                , variants = FastDict.empty
                                }

                            Just moduleExposedNames ->
                                { valuesAndFunctionsAndTypeAliasesAndEnumTypes =
                                    moduleExposedNames.enumTypesExposingVariants
                                        |> FastDict.foldl
                                            (\enumTypeName _ namesSoFar ->
                                                namesSoFar
                                                    |> FastSet.insert enumTypeName
                                            )
                                            moduleExposedNames.valueOrFunctionOrTypeAliasNames
                                , variants =
                                    moduleExposedNames.enumTypesExposingVariants
                                        |> FastDict.foldl
                                            (\_ variantNames variantsSoFar ->
                                                FastDict.union variantNames variantsSoFar
                                            )
                                            FastDict.empty
                                }
                in
                moduleContextMerge
                    (moduleContextMerge
                        { variantLookup =
                            syntaxImport.exposedVariants
                                |> FastDict.foldl
                                    (\variantName variantInfo dictSoFar ->
                                        dictSoFar
                                            |> FastDict.insert ( [], variantName )
                                                { moduleOrigin = syntaxImport.moduleName
                                                , valueCount = variantInfo.valueCount
                                                }
                                    )
                                    FastDict.empty
                        , valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                            syntaxImport.exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes
                                |> FastSet.foldl
                                    (\expose dictSoFar ->
                                        dictSoFar
                                            |> FastDict.insert ( [], expose )
                                                syntaxImport.moduleName
                                    )
                                    FastDict.empty
                        }
                        (case syntaxImport.alias of
                            Nothing ->
                                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                    importedModuleMembers.valuesAndFunctionsAndTypeAliasesAndEnumTypes
                                        |> FastSet.foldl
                                            (\exposeFromImportedModule dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( syntaxImport.moduleName, exposeFromImportedModule )
                                                        syntaxImport.moduleName
                                            )
                                            FastDict.empty
                                , variantLookup =
                                    importedModuleMembers.variants
                                        |> FastDict.foldl
                                            (\exposeFromImportedModule variantInfo dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( syntaxImport.moduleName, exposeFromImportedModule )
                                                        { moduleOrigin = syntaxImport.moduleName
                                                        , valueCount = variantInfo.valueCount
                                                        }
                                            )
                                            FastDict.empty
                                }

                            Just importAlias ->
                                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                    importedModuleMembers.valuesAndFunctionsAndTypeAliasesAndEnumTypes
                                        |> FastSet.foldl
                                            (\exposeFromImportedModule dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( [ importAlias ], exposeFromImportedModule )
                                                        syntaxImport.moduleName
                                            )
                                            FastDict.empty
                                , variantLookup =
                                    importedModuleMembers.variants
                                        |> FastDict.foldl
                                            (\exposeFromImportedModule variantInfo dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( [ importAlias ], exposeFromImportedModule )
                                                        { moduleOrigin = syntaxImport.moduleName
                                                        , valueCount = variantInfo.valueCount
                                                        }
                                            )
                                            FastDict.empty
                                }
                        )
                    )
                    soFar
            )
            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                FastDict.empty
            , variantLookup =
                FastDict.empty
            }


moduleContextMerge : ModuleContext -> ModuleContext -> ModuleContext
moduleContextMerge a b =
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
        FastDict.union
            a.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
            b.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
    , variantLookup =
        FastDict.union
            a.variantLookup
            b.variantLookup
    }


implicitImports :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
implicitImports =
    [ { moduleName = [ "Basics" ]
      , alias = Nothing
      , exposedVariants =
            FastDict.fromList
                [ ( "EQ", { valueCount = 0 } )
                , ( "LT", { valueCount = 0 } )
                , ( "GT", { valueCount = 0 } )
                , ( "True", { valueCount = 0 } )
                , ( "False", { valueCount = 0 } )
                ]
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList
                [ "Int"
                , "Float"
                , "toFloat"
                , "round"
                , "floor"
                , "ceiling"
                , "truncate"
                , "max"
                , "min"
                , "compare"
                , "Order"
                , "Bool"
                , "not"
                , "xor"
                , "modBy"
                , "remainderBy"
                , "negate"
                , "abs"
                , "clamp"
                , "sqrt"
                , "logBase"
                , "e"
                , "pi"
                , "cos"
                , "sin"
                , "tan"
                , "acos"
                , "asin"
                , "atan"
                , "atan2"
                , "degrees"
                , "radians"
                , "turns"
                , "toPolar"
                , "fromPolar"
                , "isNaN"
                , "isInfinite"
                , "identity"
                , "always"
                , "Never"
                , "never"
                ]
      }
    , { moduleName = [ "List" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "List" ]
      }
    , { moduleName = [ "Maybe" ]
      , alias = Nothing
      , exposedVariants =
            FastDict.fromList
                [ ( "Just", { valueCount = 1 } )
                , ( "Nothing", { valueCount = 0 } )
                ]
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Maybe" ]
      }
    , { moduleName = [ "Result" ]
      , alias = Nothing
      , exposedVariants =
            FastDict.fromList
                [ ( "Ok", { valueCount = 1 } )
                , ( "Err", { valueCount = 1 } )
                ]
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Result" ]
      }
    , { moduleName = [ "String" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "String" ]
      }
    , { moduleName = [ "Char" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Char" ]
      }
    , { moduleName = [ "Tuple" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.empty
      }
    , { moduleName = [ "Debug" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.empty
      }
    , { moduleName = [ "Platform" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Program" ]
      }
    , { moduleName = [ "Platform", "Cmd" ]
      , alias = Just "Cmd"
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Cmd" ]
      }
    , { moduleName = [ "Platform", "Sub" ]
      , alias = Just "Sub"
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
            FastSet.fromList [ "Sub" ]
      }
    ]


importsCombine :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict String { valueCount : Int }
            }
importsCombine syntaxImports =
    importsCombineFrom [] syntaxImports


importsCombineFrom :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict String { valueCount : Int }
            }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict String { valueCount : Int }
            }
importsCombineFrom soFar syntaxImports =
    case syntaxImports of
        [] ->
            soFar

        [ onlyImport ] ->
            onlyImport :: soFar

        import0 :: import1 :: import2Up ->
            if import0.moduleName == import1.moduleName then
                importsCombineFrom soFar
                    (importsMerge import0 import1
                        :: import2Up
                    )

            else
                importsCombineFrom
                    (import0 :: soFar)
                    (import1 :: import2Up)


importsMerge :
    { moduleName : Elm.Syntax.ModuleName.ModuleName
    , alias : Maybe String
    , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
        FastSet.Set String
    , exposedVariants :
        FastDict.Dict String { valueCount : Int }
    }
    ->
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
    ->
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
importsMerge earlier later =
    { moduleName = earlier.moduleName
    , alias =
        case earlier.alias of
            Just alias ->
                alias |> Just

            Nothing ->
                later.alias
    , exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes =
        FastSet.union
            earlier.exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes
            later.exposedValuesAndFunctionsAndTypeAliasesAndEnumTypes
    , exposedVariants =
        FastDict.union
            earlier.exposedVariants
            later.exposedVariants
    }


typeContainedRecords :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    ->
        FastSet.Set
            -- sorted field names
            (List String)
typeContainedRecords (Elm.Syntax.Node.Node _ syntaxType) =
    -- IGNORE TCO
    case syntaxType of
        Elm.Syntax.TypeAnnotation.GenericType _ ->
            FastSet.empty

        Elm.Syntax.TypeAnnotation.Unit ->
            FastSet.empty

        Elm.Syntax.TypeAnnotation.GenericRecord _ _ ->
            -- because not supported
            FastSet.empty

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inNode outNode ->
            FastSet.union
                (inNode |> typeContainedRecords)
                (outNode |> typeContainedRecords)

        Elm.Syntax.TypeAnnotation.Tupled parts ->
            parts
                |> listMapToFastSetsAndUnify typeContainedRecords

        Elm.Syntax.TypeAnnotation.Record fields ->
            FastSet.insert
                (fields
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ name, _ )) ->
                            name |> lowercaseNameSanitizeForGleam
                        )
                    |> List.sort
                )
                (fields
                    |> listMapToFastSetsAndUnify
                        (\(Elm.Syntax.Node.Node _ ( _, value )) ->
                            value |> typeContainedRecords
                        )
                )

        Elm.Syntax.TypeAnnotation.Typed _ arguments ->
            arguments
                |> listMapToFastSetsAndUnify typeContainedRecords


enumTypeDeclaration :
    ModuleContext
    -> Elm.Syntax.Type.Type
    ->
        Result
            String
            { name : String
            , parameters : List String
            , variants : FastDict.Dict String (List GleamType)
            }
enumTypeDeclaration moduleOriginLookup syntaxEnumType =
    Result.map
        (\variants ->
            { name =
                syntaxEnumType.name
                    |> Elm.Syntax.Node.value
            , parameters =
                syntaxEnumType.generics
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ parameter) ->
                            parameter |> lowercaseNameSanitizeForGleam
                        )
            , variants = variants |> FastDict.fromList
            }
        )
        (syntaxEnumType.constructors
            |> listMapAndCombineOk
                (\(Elm.Syntax.Node.Node _ syntaxVariant) ->
                    Result.map
                        (\values ->
                            ( syntaxVariant.name |> Elm.Syntax.Node.value
                            , values
                            )
                        )
                        (syntaxVariant.arguments
                            |> listMapAndCombineOk
                                (\value ->
                                    value |> type_ moduleOriginLookup
                                )
                        )
                )
        )


gleamTypeParametersToString : List String -> String
gleamTypeParametersToString gleamTypeParameters =
    case gleamTypeParameters of
        [] ->
            ""

        parameter0 :: parameter1Up ->
            "("
                ++ ((parameter0 :: parameter1Up)
                        |> String.join ", "
                   )
                ++ ")"


printGleamChoiceTypeDeclaration :
    { name : String
    , parameters : List String
    , variants : FastDict.Dict String (List GleamType)
    }
    -> Print
printGleamChoiceTypeDeclaration gleamEnumType =
    Print.exactly
        ("type "
            ++ gleamEnumType.name
            ++ (gleamEnumType.parameters
                    |> gleamTypeParametersToString
               )
            ++ " {"
        )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (gleamEnumType.variants
                            |> FastDict.toList
                            |> Print.listMapAndIntersperseAndFlatten
                                (\( name, values ) ->
                                    printGleamVariant
                                        { name = name
                                        , values = values
                                        }
                                )
                                Print.linebreakIndented
                        )
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy (Print.exactly "}")


printGleamVariant : { name : String, values : List GleamType } -> Print
printGleamVariant gleamVariant =
    Print.exactly gleamVariant.name
        |> Print.followedBy
            (case gleamVariant.values of
                [] ->
                    Print.empty

                value0 :: value1Up ->
                    let
                        valuePrints : List Print
                        valuePrints =
                            (value0 :: value1Up)
                                |> List.map printGleamTypeNotParenthesized

                        fullLineSpread : Print.LineSpread
                        fullLineSpread =
                            valuePrints
                                |> Print.lineSpreadListMapAndCombine Print.lineSpread
                    in
                    Print.exactly "("
                        |> Print.followedBy
                            (Print.withIndentAtNextMultipleOf4
                                (Print.emptyOrLinebreakIndented fullLineSpread
                                    |> Print.followedBy
                                        (valuePrints
                                            |> Print.listMapAndIntersperseAndFlatten
                                                (\valuePrint ->
                                                    Print.withIndentAtNextMultipleOf4 valuePrint
                                                )
                                                (Print.exactly ","
                                                    |> Print.followedBy
                                                        (Print.spaceOrLinebreakIndented fullLineSpread)
                                                )
                                        )
                                )
                            )
                        |> Print.followedBy
                            (Print.emptyOrLinebreakIndented fullLineSpread)
                        |> Print.followedBy (Print.exactly ")")
            )


typeAliasDeclaration :
    ModuleContext
    -> Elm.Syntax.TypeAlias.TypeAlias
    ->
        Result
            String
            { name : String
            , parameters : List String
            , type_ : GleamType
            }
typeAliasDeclaration moduleOriginLookup syntaxTypeAlias =
    Result.map
        (\aliasedType ->
            { name =
                syntaxTypeAlias.name
                    |> Elm.Syntax.Node.value
            , parameters =
                syntaxTypeAlias.generics
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ parameter) ->
                            parameter |> lowercaseNameSanitizeForGleam
                        )
            , type_ = aliasedType
            }
        )
        (syntaxTypeAlias.typeAnnotation
            |> type_ moduleOriginLookup
        )


printGleamTypeAliasDeclaration :
    { name : String
    , parameters : List String
    , type_ : GleamType
    }
    -> Print
printGleamTypeAliasDeclaration gleamTypeAliasDeclaration =
    Print.exactly
        ("type "
            ++ gleamTypeAliasDeclaration.name
            ++ (gleamTypeAliasDeclaration.parameters
                    |> gleamTypeParametersToString
               )
            ++ " ="
        )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (gleamTypeAliasDeclaration.type_
                            |> printGleamTypeNotParenthesized
                        )
                )
            )


printGleamRecordTypeDeclaration : List String -> Print
printGleamRecordTypeDeclaration gleamRecordFields =
    let
        recordName : String
        recordName =
            generatedGleamRecordNameForFields gleamRecordFields

        fieldsPrint : Print
        fieldsPrint =
            gleamRecordFields
                |> Print.listMapAndIntersperseAndFlatten
                    (\fieldName ->
                        Print.exactly (fieldName ++ ": " ++ fieldName)
                    )
                    (Print.exactly ","
                        |> Print.followedBy Print.linebreakIndented
                    )

        fullLineSpread : Print.LineSpread
        fullLineSpread =
            fieldsPrint |> Print.lineSpread
    in
    Print.exactly
        ("type "
            ++ recordName
            ++ (gleamRecordFields
                    |> gleamTypeParametersToString
               )
            ++ " {"
        )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented fullLineSpread
                    |> Print.followedBy
                        (Print.exactly (recordName ++ "("))
                    |> Print.followedBy
                        (Print.withIndentAtNextMultipleOf4
                            (Print.spaceOrLinebreakIndented fullLineSpread
                                |> Print.followedBy fieldsPrint
                            )
                        )
                    |> Print.followedBy
                        (Print.spaceOrLinebreakIndented fullLineSpread)
                    |> Print.followedBy (Print.exactly ")")
                )
            )
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented fullLineSpread)
        |> Print.followedBy (Print.exactly "}")


type_ :
    ModuleContext
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Result String GleamType
type_ moduleOriginLookup (Elm.Syntax.Node.Node _ syntaxType) =
    -- IGNORE TCO
    case syntaxType of
        Elm.Syntax.TypeAnnotation.Unit ->
            Ok gleamTypeNil

        Elm.Syntax.TypeAnnotation.GenericType variable ->
            Ok (GleamTypeVariable (variable |> lowercaseNameSanitizeForGleam))

        Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ reference) typedArguments ->
            let
                ( qualification, name ) =
                    reference
            in
            case moduleOriginLookup.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup |> FastDict.get reference of
                Nothing ->
                    Err
                        ("could not find module origin of the type reference "
                            ++ qualifiedToString
                                { qualification = qualification
                                , name = name
                                }
                        )

                Just moduleOrigin ->
                    Result.map
                        (\arguments ->
                            let
                                gleamReference : { moduleOrigin : Maybe String, name : String }
                                gleamReference =
                                    case
                                        { moduleOrigin = moduleOrigin
                                        , name = name
                                        }
                                            |> referenceToCoreGleam
                                    of
                                        Just coreGleam ->
                                            coreGleam

                                        Nothing ->
                                            { moduleOrigin = Nothing
                                            , name =
                                                { moduleOrigin = moduleOrigin
                                                , name = name
                                                }
                                                    |> uppercaseReferenceToGleamName
                                            }
                            in
                            GleamTypeConstruct
                                { moduleOrigin = gleamReference.moduleOrigin
                                , name = gleamReference.name
                                , arguments = arguments
                                }
                        )
                        (typedArguments
                            |> listMapAndCombineOk
                                (\argument -> argument |> type_ moduleOriginLookup)
                        )

        Elm.Syntax.TypeAnnotation.Tupled parts ->
            case parts of
                [] ->
                    -- should be handled by Unit
                    Ok gleamTypeNil

                [ inParens ] ->
                    type_ moduleOriginLookup inParens

                [ tuplePart0, tuplePart1 ] ->
                    Result.map2
                        (\part0 part1 ->
                            GleamTypeTuple
                                { part0 = part0
                                , part1 = part1
                                , part2Up = []
                                }
                        )
                        (tuplePart0 |> type_ moduleOriginLookup)
                        (tuplePart1 |> type_ moduleOriginLookup)

                [ tuplePart0, tuplePart1, tuplePart2 ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            GleamTypeTuple
                                { part0 = part0
                                , part1 = part1
                                , part2Up = [ part2 ]
                                }
                        )
                        (tuplePart0 |> type_ moduleOriginLookup)
                        (tuplePart1 |> type_ moduleOriginLookup)
                        (tuplePart2 |> type_ moduleOriginLookup)

                _ :: _ :: _ :: _ :: _ ->
                    Err "too many tuple parts"

        Elm.Syntax.TypeAnnotation.Record recordFields ->
            Result.map
                (\fields ->
                    let
                        fieldAsFastDict : FastDict.Dict String GleamType
                        fieldAsFastDict =
                            FastDict.fromList fields
                    in
                    GleamTypeConstruct
                        { moduleOrigin = Nothing
                        , name =
                            generatedGleamRecordNameForFields
                                (fieldAsFastDict |> FastDict.keys)
                        , arguments =
                            fieldAsFastDict
                                |> FastDict.values
                        }
                )
                (recordFields
                    |> listMapAndCombineOk
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ name, valueNode )) ->
                            Result.map
                                (\value ->
                                    ( name |> lowercaseNameSanitizeForGleam
                                    , value
                                    )
                                )
                                (valueNode |> type_ moduleOriginLookup)
                        )
                )

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inputNode outputNode ->
            Result.map2
                (\input0 outputExpandedReverse ->
                    case outputExpandedReverse of
                        output :: inputLastTo1 ->
                            GleamTypeFunction
                                { input = input0 :: (inputLastTo1 |> List.reverse)
                                , output = output
                                }

                        -- too lazy to make it non-empty
                        [] ->
                            input0
                )
                (inputNode |> type_ moduleOriginLookup)
                (outputNode
                    |> typeExpandFunctionOutputReverse
                    |> listMapAndCombineOk
                        (\partOfOutput ->
                            type_ moduleOriginLookup partOfOutput
                        )
                )

        Elm.Syntax.TypeAnnotation.GenericRecord _ _ ->
            Err "extensible record types are not supported"


typeExpandFunctionOutputReverse :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
typeExpandFunctionOutputReverse typeNode =
    typeExpandFunctionOutputIntoReverse [] typeNode


typeExpandFunctionOutputIntoReverse :
    List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
typeExpandFunctionOutputIntoReverse soFarReverse (Elm.Syntax.Node.Node fullRange syntaxType) =
    case syntaxType of
        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inputNode outputNode ->
            typeExpandFunctionOutputIntoReverse
                (inputNode :: soFarReverse)
                outputNode

        otherType ->
            Elm.Syntax.Node.Node fullRange otherType :: soFarReverse


gleamTypeNil : GleamType
gleamTypeNil =
    GleamTypeConstruct
        { moduleOrigin = Nothing
        , name = "Nil"
        , arguments = []
        }


printGleamTypeNotParenthesized : GleamType -> Print
printGleamTypeNotParenthesized gleamType =
    -- IGNORE TCO
    case gleamType of
        GleamTypeVariable variable ->
            Print.exactly variable

        GleamTypeConstruct typeConstruct ->
            printGleamTypeConstruct typeConstruct

        GleamTypeTuple parts ->
            printGleamTypeTuple parts

        GleamTypeFunction typeFunction ->
            printGleamTypeFunction typeFunction


printGleamTypeFunction :
    { input : List GleamType, output : GleamType }
    -> Print
printGleamTypeFunction typeFunction =
    let
        inputPrints : List Print
        inputPrints =
            typeFunction.input
                |> List.map printGleamTypeNotParenthesized

        outputPrint : Print
        outputPrint =
            printGleamTypeNotParenthesized
                typeFunction.output

        inputLineSpread : Print.LineSpread
        inputLineSpread =
            inputPrints
                |> Print.lineSpreadListMapAndCombine
                    Print.lineSpread

        fullLineSpread : Print.LineSpread
        fullLineSpread =
            inputLineSpread
                |> Print.lineSpreadMergeWith
                    (\() ->
                        outputPrint |> Print.lineSpread
                    )
    in
    Print.exactly "fn("
        |> Print.followedBy
            (inputPrints
                |> Print.listMapAndIntersperseAndFlatten
                    (\inputPrint -> Print.withIndentIncreasedBy 3 inputPrint)
                    (Print.exactly ","
                        |> Print.followedBy
                            (Print.spaceOrLinebreakIndented inputLineSpread)
                    )
            )
        |> Print.followedBy
            (Print.emptyOrLinebreakIndented inputLineSpread)
        |> Print.followedBy (Print.exactly ") ->")
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented fullLineSpread)
        |> Print.followedBy outputPrint


printGleamTypeTuple :
    { part0 : GleamType, part1 : GleamType, part2Up : List GleamType }
    -> Print
printGleamTypeTuple parts =
    let
        part0Print : Print
        part0Print =
            parts.part0 |> printGleamTypeNotParenthesized

        part1Print : Print
        part1Print =
            parts.part1 |> printGleamTypeNotParenthesized

        part2UpPrints : List Print
        part2UpPrints =
            parts.part2Up
                |> List.map printGleamTypeNotParenthesized
    in
    Print.exactly "#("
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                ((part0Print :: part1Print :: part2UpPrints)
                    |> Print.listIntersperseAndFlatten
                        (Print.exactly ","
                            |> Print.followedBy Print.linebreakIndented
                        )
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy (Print.exactly ")")


printGleamTypeConstruct :
    { moduleOrigin : Maybe String
    , name : String
    , arguments : List GleamType
    }
    -> Print
printGleamTypeConstruct typeConstruct =
    let
        referencePrint : Print
        referencePrint =
            Print.exactly
                (gleamReferenceToString
                    { moduleOrigin = typeConstruct.moduleOrigin
                    , name = typeConstruct.name
                    }
                )
    in
    case typeConstruct.arguments of
        [] ->
            referencePrint

        argument0 :: argument1Up ->
            let
                argumentPrints : List Print
                argumentPrints =
                    (argument0 :: argument1Up)
                        |> List.map printGleamTypeNotParenthesized

                fullLineSpread : Print.LineSpread
                fullLineSpread =
                    argumentPrints
                        |> Print.lineSpreadListMapAndCombine Print.lineSpread
            in
            referencePrint
                |> Print.followedBy (Print.exactly "(")
                |> Print.followedBy
                    (Print.withIndentAtNextMultipleOf4
                        (Print.emptyOrLinebreakIndented fullLineSpread
                            |> Print.followedBy
                                (argumentPrints
                                    |> Print.listIntersperseAndFlatten
                                        (Print.exactly ","
                                            |> Print.followedBy
                                                (Print.spaceOrLinebreakIndented fullLineSpread)
                                        )
                                )
                        )
                    )
                |> Print.followedBy
                    (Print.emptyOrLinebreakIndented fullLineSpread)
                |> Print.followedBy
                    (Print.exactly ")")


gleamReferenceToString :
    { moduleOrigin : Maybe String
    , name : String
    }
    -> String
gleamReferenceToString reference =
    case reference.moduleOrigin of
        Nothing ->
            reference.name

        Just moduleName ->
            moduleName
                ++ "."
                ++ reference.name


qualifiedToString :
    { qualification : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    -> String
qualifiedToString reference =
    case reference.qualification of
        [] ->
            reference.name

        qualificationPart0 :: qualificationPart1Up ->
            ((qualificationPart0 :: qualificationPart1Up)
                |> String.join "."
            )
                ++ "."
                ++ reference.name


stringFirstCharToLower : String -> String
stringFirstCharToLower string =
    case string |> String.uncons of
        Nothing ->
            ""

        Just ( headChar, tailString ) ->
            String.cons (Char.toLower headChar) tailString


stringFirstCharToUpper : String -> String
stringFirstCharToUpper string =
    case string |> String.uncons of
        Nothing ->
            ""

        Just ( headChar, tailString ) ->
            String.cons (Char.toUpper headChar) tailString


stringFirstCharIsUpper : String -> Bool
stringFirstCharIsUpper string =
    case string |> String.uncons of
        Nothing ->
            False

        Just ( headChar, _ ) ->
            headChar |> Char.isUpper


printGleamString : String -> Print
printGleamString stringContent =
    let
        singleDoubleQuotedStringContentEscaped : String
        singleDoubleQuotedStringContentEscaped =
            stringContent
                |> String.foldl
                    (\contentChar soFar ->
                        soFar ++ singleDoubleQuotedStringCharToEscaped contentChar ++ ""
                    )
                    ""
    in
    Print.exactly ("\"" ++ singleDoubleQuotedStringContentEscaped ++ "\"")


singleDoubleQuotedStringCharToEscaped : Char -> String
singleDoubleQuotedStringCharToEscaped character =
    case character of
        '"' ->
            "\\\""

        '\\' ->
            "\\\\"

        '\t' ->
            "\\t"

        '\n' ->
            "\\n"

        '$' ->
            "\\$"

        '\u{000D}' ->
            "\\r"

        otherCharacter ->
            if characterIsNotPrint otherCharacter then
                "\\u{" ++ characterHex otherCharacter ++ "}"

            else
                String.fromChar otherCharacter


intToHexString : Int -> String
intToHexString int =
    -- IGNORE TCO
    if int < 16 then
        unsafeHexDigitIntToString int

    else
        intToHexString (int // 16)
            ++ unsafeHexDigitIntToString (int |> Basics.remainderBy 16)
            ++ ""


unsafeHexDigitIntToString : Int -> String
unsafeHexDigitIntToString int =
    case int of
        0 ->
            "0"

        1 ->
            "1"

        2 ->
            "2"

        3 ->
            "3"

        4 ->
            "4"

        5 ->
            "5"

        6 ->
            "6"

        7 ->
            "7"

        8 ->
            "8"

        9 ->
            "9"

        10 ->
            "A"

        11 ->
            "B"

        12 ->
            "C"

        13 ->
            "D"

        14 ->
            "E"

        -- 15
        _ ->
            "F"


characterHex : Char -> String
characterHex character =
    String.toUpper
        (intToHexString (Char.toCode character)
            |> String.padLeft 8 '0'
        )


characterIsNotPrint : Char -> Bool
characterIsNotPrint character =
    if
        -- Unicode.getCategory is very expensive so we shortcut if at all possible
        charIsLatinAlphaNumOrUnderscoreFast character
            || (character == ' ')
            || (character == '.')
            || (character == '!')
            || (character == '?')
            || (character == '-')
            || (character == ':')
    then
        False

    else
        case Unicode.getCategory character of
            Nothing ->
                True

            Just category ->
                case category of
                    Unicode.SeparatorLine ->
                        True

                    Unicode.SeparatorParagraph ->
                        True

                    Unicode.OtherControl ->
                        True

                    Unicode.OtherFormat ->
                        True

                    Unicode.OtherSurrogate ->
                        True

                    Unicode.OtherPrivateUse ->
                        True

                    Unicode.OtherNotAssigned ->
                        True

                    Unicode.LetterUppercase ->
                        False

                    Unicode.LetterLowercase ->
                        False

                    Unicode.LetterTitlecase ->
                        False

                    Unicode.MarkNonSpacing ->
                        False

                    Unicode.MarkSpacingCombining ->
                        False

                    Unicode.MarkEnclosing ->
                        False

                    Unicode.NumberDecimalDigit ->
                        False

                    Unicode.NumberLetter ->
                        False

                    Unicode.NumberOther ->
                        False

                    Unicode.SeparatorSpace ->
                        True

                    Unicode.LetterModifier ->
                        False

                    Unicode.LetterOther ->
                        False

                    Unicode.PunctuationConnector ->
                        False

                    Unicode.PunctuationDash ->
                        False

                    Unicode.PunctuationOpen ->
                        False

                    Unicode.PunctuationClose ->
                        False

                    Unicode.PunctuationInitialQuote ->
                        False

                    Unicode.PunctuationFinalQuote ->
                        False

                    Unicode.PunctuationOther ->
                        False

                    Unicode.SymbolMath ->
                        False

                    Unicode.SymbolCurrency ->
                        False

                    Unicode.SymbolModifier ->
                        False

                    Unicode.SymbolOther ->
                        False


charLiteral : Char -> String
charLiteral charContent =
    String.fromInt (charContent |> Char.toCode)


charCodeIsLower : Int -> Bool
charCodeIsLower code =
    0x61 <= code && code <= 0x7A


charCodeIsUpper : Int -> Bool
charCodeIsUpper code =
    code <= 0x5A && 0x41 <= code


charCodeIsDigit : Int -> Bool
charCodeIsDigit code =
    code <= 0x39 && 0x30 <= code


charIsLatinAlphaNumOrUnderscoreFast : Char -> Bool
charIsLatinAlphaNumOrUnderscoreFast c =
    let
        code : Int
        code =
            Char.toCode c
    in
    charCodeIsLower code
        || charCodeIsUpper code
        || charCodeIsDigit code
        || -- (c == '_')
           (code == 95)


pattern :
    ModuleContext
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    ->
        Result
            String
            { pattern : GleamPattern
            , introducedVariables : FastSet.Set String
            , recordFieldDestructuringsToAdd :
                FastDict.Dict String { recordVariable : String }
            }
pattern moduleOriginLookup (Elm.Syntax.Node.Node _ syntaxPattern) =
    -- IGNORE TCO
    case syntaxPattern of
        Elm.Syntax.Pattern.AllPattern ->
            Ok
                { pattern = GleamPatternIgnore
                , introducedVariables = FastSet.empty
                , recordFieldDestructuringsToAdd = FastDict.empty
                }

        Elm.Syntax.Pattern.UnitPattern ->
            Ok
                { pattern = GleamPatternIgnore
                , introducedVariables = FastSet.empty
                , recordFieldDestructuringsToAdd = FastDict.empty
                }

        Elm.Syntax.Pattern.CharPattern charValue ->
            Ok
                { pattern = GleamPatternChar charValue
                , introducedVariables = FastSet.empty
                , recordFieldDestructuringsToAdd = FastDict.empty
                }

        Elm.Syntax.Pattern.StringPattern stringValue ->
            Ok
                { pattern = GleamPatternString stringValue
                , introducedVariables = FastSet.empty
                , recordFieldDestructuringsToAdd = FastDict.empty
                }

        Elm.Syntax.Pattern.IntPattern intValue ->
            Ok
                { pattern = GleamPatternNumber (intValue |> Basics.toFloat)
                , introducedVariables = FastSet.empty
                , recordFieldDestructuringsToAdd = FastDict.empty
                }

        Elm.Syntax.Pattern.HexPattern intValue ->
            Ok
                { pattern = GleamPatternNumber (intValue |> Basics.toFloat)
                , introducedVariables = FastSet.empty
                , recordFieldDestructuringsToAdd = FastDict.empty
                }

        Elm.Syntax.Pattern.FloatPattern _ ->
            Err "float pattern is invalid syntax"

        Elm.Syntax.Pattern.VarPattern variableName ->
            let
                sanitizedVariableName : String
                sanitizedVariableName =
                    variableName |> lowercaseNameSanitizeForGleam
            in
            Ok
                { pattern =
                    GleamPatternVariable sanitizedVariableName
                , introducedVariables =
                    FastSet.singleton sanitizedVariableName
                , recordFieldDestructuringsToAdd = FastDict.empty
                }

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            pattern moduleOriginLookup inParens

        Elm.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [] ->
                    -- should be covered by UnitPattern
                    Ok
                        { pattern = GleamPatternIgnore
                        , introducedVariables = FastSet.empty
                        , recordFieldDestructuringsToAdd = FastDict.empty
                        }

                [ inParens ] ->
                    -- should be covered by ParenthesizedPattern
                    pattern moduleOriginLookup inParens

                [ part0Node, part1Node ] ->
                    Result.map2
                        (\part0 part1 ->
                            { pattern =
                                GleamPatternTuple
                                    { part0 = part0.pattern
                                    , part1 = part1.pattern
                                    , part2Up = []
                                    }
                            , introducedVariables =
                                FastSet.union
                                    part0.introducedVariables
                                    part1.introducedVariables
                            , recordFieldDestructuringsToAdd =
                                FastDict.union
                                    part0.recordFieldDestructuringsToAdd
                                    part1.recordFieldDestructuringsToAdd
                            }
                        )
                        (part0Node |> pattern moduleOriginLookup)
                        (part1Node |> pattern moduleOriginLookup)

                [ part0Node, part1Node, part2Node ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            { pattern =
                                GleamPatternTuple
                                    { part0 = part0.pattern
                                    , part1 = part1.pattern
                                    , part2Up = [ part2.pattern ]
                                    }
                            , introducedVariables =
                                FastSet.union
                                    part0.introducedVariables
                                    (FastSet.union
                                        part1.introducedVariables
                                        part2.introducedVariables
                                    )
                            , recordFieldDestructuringsToAdd =
                                FastDict.union
                                    part0.recordFieldDestructuringsToAdd
                                    (FastDict.union
                                        part1.recordFieldDestructuringsToAdd
                                        part2.recordFieldDestructuringsToAdd
                                    )
                            }
                        )
                        (part0Node |> pattern moduleOriginLookup)
                        (part1Node |> pattern moduleOriginLookup)
                        (part2Node |> pattern moduleOriginLookup)

                _ :: _ :: _ :: _ :: _ ->
                    -- invalid syntax
                    Err "too many tuple parts"

        Elm.Syntax.Pattern.RecordPattern fields ->
            let
                fieldNames : List String
                fieldNames =
                    fields
                        |> List.map
                            (\(Elm.Syntax.Node.Node _ fieldName) ->
                                fieldName |> lowercaseNameSanitizeForGleam
                            )

                generatedRecordVariable : String
                generatedRecordVariable =
                    "generated_" ++ (fieldNames |> String.join "_")
            in
            Ok
                { pattern = GleamPatternVariable generatedRecordVariable
                , introducedVariables =
                    FastSet.insert generatedRecordVariable
                        (fieldNames |> FastSet.fromList)
                , recordFieldDestructuringsToAdd =
                    fieldNames
                        |> List.foldl
                            (\fieldName soFar ->
                                soFar
                                    |> FastDict.insert fieldName
                                        { recordVariable = generatedRecordVariable }
                            )
                            FastDict.empty
                }

        Elm.Syntax.Pattern.UnConsPattern headPatternNode tailPatternNode ->
            let
                tailExpanded :
                    { initialElements : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
                    , tail : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
                    }
                tailExpanded =
                    tailPatternNode |> patternConsExpand
            in
            resultAndThen3
                (\initialElement0 initialElement1Up tail ->
                    let
                        introducedVariables : FastSet.Set String
                        introducedVariables =
                            FastSet.union
                                initialElement0.introducedVariables
                                (FastSet.union
                                    (initialElement1Up
                                        |> listMapToFastSetsAndUnify .introducedVariables
                                    )
                                    (case tail of
                                        Nothing ->
                                            FastSet.empty

                                        Just tailVariable ->
                                            FastSet.singleton tailVariable
                                    )
                                )
                    in
                    Ok
                        { pattern =
                            GleamPatternListCons
                                { initialElement0 = initialElement0.pattern
                                , initialElement1Up =
                                    initialElement1Up
                                        |> List.map .pattern
                                , tail = tail
                                }
                        , introducedVariables = introducedVariables
                        , recordFieldDestructuringsToAdd =
                            FastDict.union
                                initialElement0.recordFieldDestructuringsToAdd
                                (initialElement1Up
                                    |> listMapToFastDictsAndUnify .recordFieldDestructuringsToAdd
                                )
                        }
                )
                (headPatternNode |> pattern moduleOriginLookup)
                (tailExpanded.initialElements
                    |> listMapAndCombineOk
                        (\initialElement -> initialElement |> pattern moduleOriginLookup)
                )
                (case tailExpanded.tail |> Elm.Syntax.Node.value of
                    Elm.Syntax.Pattern.AllPattern ->
                        Ok Nothing

                    Elm.Syntax.Pattern.VarPattern name ->
                        Ok (Just (name |> lowercaseNameSanitizeForGleam))

                    _ ->
                        Err "gleam only allows ignore (`_`) or variable pattern as list tail"
                )

        Elm.Syntax.Pattern.ListPattern elementPatterns ->
            Result.map
                (\elements ->
                    { pattern =
                        GleamPatternListExact (elements |> List.map .pattern)
                    , introducedVariables =
                        elements
                            |> listMapToFastSetsAndUnify .introducedVariables
                    , recordFieldDestructuringsToAdd =
                        elements
                            |> listMapToFastDictsAndUnify .recordFieldDestructuringsToAdd
                    }
                )
                (elementPatterns
                    |> listMapAndCombineOk
                        (\element -> element |> pattern moduleOriginLookup)
                )

        Elm.Syntax.Pattern.NamedPattern syntaxQualifiedNameRef argumentPatterns ->
            Result.map2
                (\values reference ->
                    { pattern =
                        GleamPatternVariant
                            { moduleOrigin = reference.moduleOrigin
                            , name = reference.name
                            , values = values |> List.map .pattern
                            }
                    , introducedVariables =
                        values
                            |> listMapToFastSetsAndUnify .introducedVariables
                    , recordFieldDestructuringsToAdd =
                        values
                            |> listMapToFastDictsAndUnify .recordFieldDestructuringsToAdd
                    }
                )
                (argumentPatterns
                    |> listMapAndCombineOk
                        (\argument -> argument |> pattern moduleOriginLookup)
                )
                (case moduleOriginLookup.variantLookup |> FastDict.get ( syntaxQualifiedNameRef.moduleName, syntaxQualifiedNameRef.name ) of
                    Nothing ->
                        Err
                            ("could not find origin choice type for the variant "
                                ++ qualifiedToString
                                    { qualification = syntaxQualifiedNameRef.moduleName
                                    , name = syntaxQualifiedNameRef.name
                                    }
                            )

                    Just variantInfo ->
                        Ok
                            (case { moduleOrigin = variantInfo.moduleOrigin, name = syntaxQualifiedNameRef.name } |> referenceToCoreGleam of
                                Just gleamReference ->
                                    gleamReference

                                Nothing ->
                                    { moduleOrigin = Nothing
                                    , name =
                                        uppercaseReferenceToGleamName
                                            { moduleOrigin = variantInfo.moduleOrigin
                                            , name = syntaxQualifiedNameRef.name
                                            }
                                    }
                            )
                )

        Elm.Syntax.Pattern.AsPattern aliasedPatternNode (Elm.Syntax.Node.Node _ variable) ->
            Result.map
                (\aliasedPattern ->
                    let
                        variableDisambiguated : String
                        variableDisambiguated =
                            variable |> lowercaseNameSanitizeForGleam
                    in
                    { pattern =
                        GleamPatternAs
                            { pattern = aliasedPattern.pattern
                            , variable = variableDisambiguated
                            }
                    , introducedVariables =
                        aliasedPattern.introducedVariables
                            |> FastSet.insert variableDisambiguated
                    , recordFieldDestructuringsToAdd =
                        aliasedPattern.recordFieldDestructuringsToAdd
                    }
                )
                (aliasedPatternNode |> pattern moduleOriginLookup)


printGleamPatternListCons :
    { initialElement0 : GleamPattern
    , initialElement1Up : List GleamPattern
    , tail : Maybe String
    }
    -> Print
printGleamPatternListCons syntaxCons =
    Print.exactly "["
        |> Print.followedBy
            (syntaxCons.initialElement0
                :: syntaxCons.initialElement1Up
                |> Print.listMapAndIntersperseAndFlatten
                    (\initialElement ->
                        printGleamPatternNotParenthesized initialElement
                    )
                    (Print.exactly ", ")
            )
        |> Print.followedBy
            (Print.exactly ", ..")
        |> Print.followedBy
            (case syntaxCons.tail of
                Nothing ->
                    Print.exactly "_"

                Just variable ->
                    Print.exactly variable
            )
        |> Print.followedBy
            (Print.exactly "]")


patternConsExpand :
    Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    ->
        { initialElements : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
        , tail : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
        }
patternConsExpand patternNode =
    patternConsExpandFromInitialElementsReverse [] patternNode


patternConsExpandFromInitialElementsReverse :
    List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    ->
        { initialElements : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
        , tail : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
        }
patternConsExpandFromInitialElementsReverse initialElementsSoFarReverse (Elm.Syntax.Node.Node fulRange syntaxPattern) =
    case syntaxPattern of
        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            patternConsExpandFromInitialElementsReverse
                (headPattern :: initialElementsSoFarReverse)
                tailPattern

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            patternConsExpandFromInitialElementsReverse initialElementsSoFarReverse
                inParens

        Elm.Syntax.Pattern.AllPattern ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange Elm.Syntax.Pattern.AllPattern
            }

        Elm.Syntax.Pattern.UnitPattern ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange Elm.Syntax.Pattern.UnitPattern
            }

        Elm.Syntax.Pattern.CharPattern char ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.CharPattern char)
            }

        Elm.Syntax.Pattern.StringPattern string ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.StringPattern string)
            }

        Elm.Syntax.Pattern.IntPattern int ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.IntPattern int)
            }

        Elm.Syntax.Pattern.HexPattern int ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.HexPattern int)
            }

        Elm.Syntax.Pattern.FloatPattern float ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.FloatPattern float)
            }

        Elm.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [ part0, part1 ] ->
                    { initialElements = initialElementsSoFarReverse |> List.reverse
                    , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.TuplePattern [ part0, part1 ])
                    }

                [ part0, part1, part2 ] ->
                    { initialElements = initialElementsSoFarReverse |> List.reverse
                    , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.TuplePattern [ part0, part1, part2 ])
                    }

                [] ->
                    -- should be handled by UnitPattern
                    { initialElements = initialElementsSoFarReverse |> List.reverse
                    , tail = Elm.Syntax.Node.Node fulRange Elm.Syntax.Pattern.UnitPattern
                    }

                [ inParens ] ->
                    -- should be handled by ParenthesizedPattern
                    { initialElements = initialElementsSoFarReverse |> List.reverse
                    , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.TuplePattern [ inParens ])
                    }

                part0 :: part1 :: part2 :: part3 :: part4Up ->
                    -- should be handled by ParenthesizedPattern
                    { initialElements = initialElementsSoFarReverse |> List.reverse
                    , tail =
                        Elm.Syntax.Node.Node fulRange
                            (Elm.Syntax.Pattern.TuplePattern (part0 :: part1 :: part2 :: part3 :: part4Up))
                    }

        Elm.Syntax.Pattern.RecordPattern fields ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.RecordPattern fields)
            }

        Elm.Syntax.Pattern.ListPattern elements ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.ListPattern elements)
            }

        Elm.Syntax.Pattern.VarPattern variableName ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.VarPattern variableName)
            }

        Elm.Syntax.Pattern.NamedPattern reference parameters ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.NamedPattern reference parameters)
            }

        Elm.Syntax.Pattern.AsPattern aliasedPattern aliasName ->
            { initialElements = initialElementsSoFarReverse |> List.reverse
            , tail = Elm.Syntax.Node.Node fulRange (Elm.Syntax.Pattern.AsPattern aliasedPattern aliasName)
            }


referenceToCoreGleam :
    { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    ->
        Maybe
            { moduleOrigin : Maybe String
            , name : String
            }
referenceToCoreGleam reference =
    case reference.moduleOrigin of
        [ "Basics" ] ->
            case reference.name of
                "identity" ->
                    Just { moduleOrigin = Nothing, name = "identity" }

                "always" ->
                    Just { moduleOrigin = Nothing, name = "basics_always" }

                "compare" ->
                    Just { moduleOrigin = Just "string", name = "compare" }

                "max" ->
                    Just { moduleOrigin = Just "float", name = "max" }

                "min" ->
                    Just { moduleOrigin = Just "float", name = "min" }

                "Order" ->
                    Just { moduleOrigin = Just "order", name = "Order" }

                "LT" ->
                    Just { moduleOrigin = Just "order", name = "Lt" }

                "EQ" ->
                    Just { moduleOrigin = Just "order", name = "Eq" }

                "GT" ->
                    Just { moduleOrigin = Just "order", name = "Gt" }

                "Bool" ->
                    Just { moduleOrigin = Nothing, name = "Bool" }

                "True" ->
                    Just { moduleOrigin = Nothing, name = "True" }

                "False" ->
                    Just { moduleOrigin = Nothing, name = "False" }

                "not" ->
                    Just { moduleOrigin = Just "bool", name = "negate" }

                "xor" ->
                    Just { moduleOrigin = Just "bool", name = "exclusive_or " }

                "Int" ->
                    Just { moduleOrigin = Nothing, name = "Float" }

                "Float" ->
                    Just { moduleOrigin = Nothing, name = "Float" }

                "ceiling" ->
                    Just { moduleOrigin = Just "float", name = "ceiling" }

                "floor" ->
                    Just { moduleOrigin = Just "float", name = "floor" }

                -- yes, ceiling and floor return Float but truncate and round return Int
                "round" ->
                    Just { moduleOrigin = Nothing, name = "basics_round" }

                "truncate" ->
                    Just { moduleOrigin = Nothing, name = "basics_truncate" }

                "negate" ->
                    Just { moduleOrigin = Just "float", name = "negate" }

                "abs" ->
                    Just { moduleOrigin = Just "float", name = "absolute_value" }

                "toFloat" ->
                    Just { moduleOrigin = Just "function", name = "identity" }

                "isNaN" ->
                    Just { moduleOrigin = Nothing, name = "basics_always_false" }

                "isInfinite" ->
                    Just { moduleOrigin = Nothing, name = "basics_always_false" }

                "remainderBy" ->
                    Just { moduleOrigin = Nothing, name = "basics_remainder_by" }

                "modBy" ->
                    Just { moduleOrigin = Nothing, name = "basics_mod_by" }

                "sqrt" ->
                    Just { moduleOrigin = Just "float", name = "square_root" }

                _ ->
                    Nothing

        [ "String" ] ->
            case reference.name of
                "String" ->
                    Just { moduleOrigin = Nothing, name = "String" }

                "isEmpty" ->
                    Just { moduleOrigin = Just "string", name = "is_empty" }

                "length" ->
                    Just { moduleOrigin = Just "string", name = "length" }

                "append" ->
                    Just { moduleOrigin = Just "string", name = "concat" }

                "trim" ->
                    Just { moduleOrigin = Just "string", name = "trim" }

                "trimLeft" ->
                    Just { moduleOrigin = Just "string", name = "trim_start" }

                "trimRight" ->
                    Just { moduleOrigin = Just "string", name = "trim_end" }

                "left" ->
                    Just { moduleOrigin = Nothing, name = "string_left" }

                "right" ->
                    Just { moduleOrigin = Nothing, name = "string_right" }

                "dropLeft" ->
                    Just { moduleOrigin = Nothing, name = "string_drop_left" }

                "dropRight" ->
                    Just { moduleOrigin = Nothing, name = "string_drop_right" }

                "padLeft" ->
                    Just { moduleOrigin = Nothing, name = "string_padLeft" }

                "padRight" ->
                    Just { moduleOrigin = Nothing, name = "string_padRight" }

                "toList" ->
                    Just { moduleOrigin = Just "string", name = "to_utf_codepoints" }

                "fromList" ->
                    Just { moduleOrigin = Just "string", name = "from_utf_codepoints" }

                "concat" ->
                    Just { moduleOrigin = Just "string", name = "concat" }

                "join" ->
                    Just { moduleOrigin = Nothing, name = "string_join" }

                "filter" ->
                    Just { moduleOrigin = Nothing, name = "string_filter" }

                "any" ->
                    Just { moduleOrigin = Nothing, name = "string_any" }

                "all" ->
                    Just { moduleOrigin = Nothing, name = "string_all" }

                "map" ->
                    Just { moduleOrigin = Just "string", name = "map" }

                "repeat" ->
                    Just { moduleOrigin = Nothing, name = "string_repeat" }

                "replace" ->
                    Just { moduleOrigin = Nothing, name = "string_replace" }

                "lines" ->
                    Just { moduleOrigin = Nothing, name = "string_lines" }

                "startsWith" ->
                    Just { moduleOrigin = Just "string", name = "starts_with" }

                "endsWith" ->
                    Just { moduleOrigin = Just "string", name = "ends_with" }

                "toInt" ->
                    Just { moduleOrigin = Nothing, name = "string_to_int" }

                "toFloat" ->
                    Just { moduleOrigin = Nothing, name = "string_to_float" }

                "fromInt" ->
                    Just { moduleOrigin = Just "float", name = "to_string" }

                "fromFloat" ->
                    Just { moduleOrigin = Just "float", name = "to_string" }

                "fromChar" ->
                    Just { moduleOrigin = Nothing, name = "string_from_char" }

                "cons" ->
                    Just { moduleOrigin = Nothing, name = "string_cons" }

                "slice" ->
                    Just { moduleOrigin = Nothing, name = "string_slice" }

                "split" ->
                    Just { moduleOrigin = Nothing, name = "string_split" }

                "contains" ->
                    Just { moduleOrigin = Nothing, name = "string_contains" }

                "reverse" ->
                    Just { moduleOrigin = Just "string", name = "reverse" }

                "toLower" ->
                    Just { moduleOrigin = Just "string", name = "lowercase" }

                "toUpper" ->
                    Just { moduleOrigin = Just "string", name = "uppercase" }

                _ ->
                    Nothing

        [ "Char" ] ->
            -- represented as Int because sadly gleam has no native
            -- UtfCodepoint pattern
            case reference.name of
                "Char" ->
                    Just { moduleOrigin = Nothing, name = "Int" }

                "toCode" ->
                    Just { moduleOrigin = Just "int", name = "to_float" }

                "fromCode" ->
                    Just { moduleOrigin = Just "float", name = "truncate" }

                "toLower" ->
                    Just { moduleOrigin = Nothing, name = "char_to_lower" }

                "toUpper" ->
                    Just { moduleOrigin = Nothing, name = "char_to_upper" }

                _ ->
                    Nothing

        [ "List" ] ->
            case reference.name of
                "List" ->
                    Just { moduleOrigin = Nothing, name = "List" }

                "singleton" ->
                    Just { moduleOrigin = Just "list", name = "wrap" }

                "isEmpty" ->
                    Just { moduleOrigin = Just "list", name = "is_empty" }

                "length" ->
                    Just { moduleOrigin = Just "list", name = "length" }

                "member" ->
                    Just { moduleOrigin = Nothing, name = "list_member" }

                "minimum" ->
                    Just { moduleOrigin = Nothing, name = "list_minimum" }

                "maximum" ->
                    Just { moduleOrigin = Nothing, name = "list_maximum" }

                "sum" ->
                    Just { moduleOrigin = Just "float", name = "sum" }

                "product" ->
                    Just { moduleOrigin = Just "float", name = "product" }

                "append" ->
                    Just { moduleOrigin = Just "list", name = "append" }

                "concat" ->
                    Just { moduleOrigin = Just "list", name = "flatten" }

                "reverse" ->
                    Just { moduleOrigin = Just "list", name = "reverse" }

                "repeat" ->
                    Just { moduleOrigin = Nothing, name = "list_repeat" }

                "all" ->
                    Just { moduleOrigin = Nothing, name = "list_all" }

                "any" ->
                    Just { moduleOrigin = Nothing, name = "list_any" }

                "filter" ->
                    Just { moduleOrigin = Nothing, name = "list_filter" }

                "filterMap" ->
                    Just { moduleOrigin = Nothing, name = "list_filter_map" }

                "map" ->
                    Just { moduleOrigin = Nothing, name = "list_map" }

                "map2" ->
                    Just { moduleOrigin = Nothing, name = "list_map2" }

                "zip" ->
                    Just { moduleOrigin = Just "list", name = "zip" }

                "unzip" ->
                    Just { moduleOrigin = Just "list", name = "unzip" }

                "partition" ->
                    Just { moduleOrigin = Nothing, name = "list_partition" }

                "concatMap" ->
                    Just { moduleOrigin = Just "list", name = "list_concat_map" }

                "sort" ->
                    Just { moduleOrigin = Nothing, name = "list_sort" }

                "sortWith" ->
                    Just { moduleOrigin = Nothing, name = "list_sort_with" }

                "range" ->
                    Just { moduleOrigin = Nothing, name = "list_range" }

                "take" ->
                    Just { moduleOrigin = Nothing, name = "list_take" }

                "drop" ->
                    Just { moduleOrigin = Nothing, name = "list_drop" }

                "intersperse" ->
                    Just { moduleOrigin = Nothing, name = "list_intersperse" }

                "foldl" ->
                    Just { moduleOrigin = Nothing, name = "list_foldl" }

                "foldr" ->
                    Just { moduleOrigin = Nothing, name = "list_foldr" }

                _ ->
                    Nothing

        [ "Maybe" ] ->
            case reference.name of
                "Maybe" ->
                    Just { moduleOrigin = Just "option", name = "Option" }

                "Nothing" ->
                    Just { moduleOrigin = Just "option", name = "None" }

                "Just" ->
                    Just { moduleOrigin = Just "option", name = "Some" }

                _ ->
                    Nothing

        [ "Parser" ] ->
            case reference.name of
                -- refers to either a type or variant
                "Problem" ->
                    Just { moduleOrigin = Nothing, name = "Parser_Problem" }

                "Expecting" ->
                    Just { moduleOrigin = Nothing, name = "Parser_Expecting" }

                "ExpectingInt" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingInt" }

                "ExpectingHex" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingHex" }

                "ExpectingOctal" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingOctal" }

                "ExpectingBinary" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingBinary" }

                "ExpectingFloat" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingFloat" }

                "ExpectingNumber" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingNumber" }

                "ExpectingVariable" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingVariable" }

                "ExpectingSymbol" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingSymbol" }

                "ExpectingKeyword" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingKeyword" }

                "ExpectingEnd" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingEnd" }

                "UnexpectedChar" ->
                    Just { moduleOrigin = Nothing, name = "Parser_UnexpectedChar" }

                "BadRepeat" ->
                    Just { moduleOrigin = Nothing, name = "Parser_BadRepeat" }

                _ ->
                    Nothing

        _ ->
            Nothing


lowercaseReferenceToGleamName :
    { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    -> String
lowercaseReferenceToGleamName reference =
    (reference.moduleOrigin
        |> String.concat
        |> stringFirstCharToLower
    )
        ++ "_"
        ++ reference.name
        |> lowercaseNameSanitizeForGleam


uppercaseReferenceToGleamName :
    { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    -> String
uppercaseReferenceToGleamName reference =
    (reference.moduleOrigin
        |> String.concat
    )
        ++ (reference.name |> stringFirstCharToUpper)
        |> uppercaseNameSanitizeForGleam


printGleamPatternNotParenthesized : GleamPattern -> Print
printGleamPatternNotParenthesized gleamPattern =
    -- IGNORE TCO
    case gleamPattern of
        GleamPatternIgnore ->
            Print.exactly "_"

        GleamPatternNumber floatValue ->
            Print.exactly (gleamNumberLiteralToString floatValue)

        GleamPatternChar charValue ->
            Print.exactly (charLiteral charValue)

        GleamPatternString string ->
            printGleamString string

        GleamPatternVariable name ->
            Print.exactly name

        GleamPatternListCons gleamPatternListCons ->
            printGleamPatternListCons gleamPatternListCons

        GleamPatternListExact elements ->
            printGleamPatternListExact elements

        GleamPatternVariant patternVariant ->
            Print.exactly
                (gleamReferenceToString
                    { moduleOrigin = patternVariant.moduleOrigin
                    , name = patternVariant.name
                    }
                )
                |> Print.followedBy
                    (case patternVariant.values of
                        [] ->
                            Print.empty

                        variantValue0 :: variantValue1Up ->
                            Print.exactly "("
                                |> Print.followedBy
                                    ((variantValue0 :: variantValue1Up)
                                        |> Print.listMapAndIntersperseAndFlatten
                                            printGleamPatternNotParenthesized
                                            (Print.exactly ", ")
                                    )
                                |> Print.followedBy (Print.exactly ")")
                    )

        GleamPatternAs patternAs ->
            printGleamPatternAs patternAs

        GleamPatternTuple parts ->
            Print.exactly "#("
                |> Print.followedBy
                    ((parts.part0 :: parts.part1 :: parts.part2Up)
                        |> Print.listMapAndIntersperseAndFlatten
                            printGleamPatternNotParenthesized
                            (Print.exactly ", ")
                    )
                |> Print.followedBy (Print.exactly ")")


printGleamPatternListExact : List GleamPattern -> Print
printGleamPatternListExact elements =
    case elements of
        [] ->
            Print.exactly "[]"

        element0 :: element1Up ->
            Print.exactly "[ "
                |> Print.followedBy
                    ((element0 :: element1Up)
                        |> Print.listMapAndIntersperseAndFlatten
                            (\elementNode ->
                                Print.withIndentIncreasedBy 2
                                    (printGleamPatternNotParenthesized elementNode)
                            )
                            (Print.exactly ", ")
                    )
                |> Print.followedBy (Print.exactly " ]")


printGleamPatternAs :
    { variable : String
    , pattern : GleamPattern
    }
    -> Print
printGleamPatternAs syntaxAs =
    printGleamPatternParenthesizedIfSpaceSeparated
        syntaxAs.pattern
        |> Print.followedBy
            (Print.exactly (" as " ++ syntaxAs.variable))


printGleamExpressionRecord : FastDict.Dict String GleamExpression -> Print
printGleamExpressionRecord syntaxRecordFields =
    let
        recordName : String
        recordName =
            generatedGleamRecordNameForFields
                (syntaxRecordFields |> FastDict.keys)

        fieldsPrints : List Print
        fieldsPrints =
            syntaxRecordFields
                |> FastDict.toList
                |> List.map
                    (\( fieldName, fieldValue ) ->
                        let
                            fieldValuePrint : Print
                            fieldValuePrint =
                                printGleamExpressionNotParenthesized fieldValue
                        in
                        Print.exactly (fieldName ++ ":")
                            |> Print.followedBy
                                (Print.withIndentAtNextMultipleOf4
                                    (Print.spaceOrLinebreakIndented
                                        (fieldValuePrint |> Print.lineSpread)
                                        |> Print.followedBy fieldValuePrint
                                    )
                                )
                    )

        fullLineSpread : Print.LineSpread
        fullLineSpread =
            fieldsPrints
                |> Print.lineSpreadListMapAndCombine Print.lineSpread
    in
    Print.exactly (recordName ++ "(")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented fullLineSpread
                    |> Print.followedBy
                        (fieldsPrints
                            |> Print.listIntersperseAndFlatten
                                (Print.exactly ","
                                    |> Print.followedBy Print.linebreakIndented
                                )
                        )
                )
            )
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented fullLineSpread)
        |> Print.followedBy (Print.exactly ")")


printParenthesized : { opening : String, closing : String, inner : Print } -> Print
printParenthesized config =
    Print.exactly config.opening
        |> Print.followedBy
            (Print.withIndentIncreasedBy 1
                config.inner
            )
        |> Print.followedBy
            (Print.emptyOrLinebreakIndented
                (config.inner |> Print.lineSpread)
            )
        |> Print.followedBy (Print.exactly config.closing)


{-| Transpile a list of [`Elm.Syntax.Declaration.Declaration`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration#Declaration)s
across multiple modules to value, function and type declarations.
Declarations that use unsupported stuff like parser kernel code (directly or indirectly)
will not be present in the final declarations.
Their errors can be found alongside the valid transpiled declarations.

The given list of files must also include files from used dependencies
including `elm/core`.

-}
modules :
    List Elm.Syntax.File.File
    ->
        { errors : List String
        , declarations :
            { valuesAndFunctions :
                FastDict.Dict
                    String
                    { parameters : List (Maybe String)
                    , result : GleamExpression
                    , type_ : Maybe GleamType
                    }
            , typeAliases :
                FastDict.Dict
                    String
                    { parameters : List String
                    , type_ : GleamType
                    }
            , recordTypes : FastSet.Set (List String)
            , choiceTypes :
                FastDict.Dict
                    String
                    { parameters : List String
                    , variants : FastDict.Dict String (List GleamType)
                    }
            }
        }
modules syntaxDeclarationsIncludingOverwrittenOnes =
    let
        syntaxModules : List Elm.Syntax.File.File
        syntaxModules =
            syntaxDeclarationsIncludingOverwrittenOnes
                |> List.filter
                    (\syntaxModule ->
                        case
                            syntaxModule.moduleDefinition
                                |> Elm.Syntax.Node.value
                                |> moduleHeaderName
                        of
                            [ "Basics" ] ->
                                False

                            [ "Array" ] ->
                                False

                            -- https://github.com/elm/core/blob/1.0.5/src/Elm/JsArray.elm
                            [ "Elm", "JsArray" ] ->
                                False

                            [ "Bitwise" ] ->
                                False

                            [ "Debug" ] ->
                                False

                            [ "Char" ] ->
                                False

                            [ "String" ] ->
                                False

                            [ "List" ] ->
                                False

                            [ "Dict" ] ->
                                False

                            [ "Set" ] ->
                                False

                            [ "Platform" ] ->
                                False

                            [ "Platform", "Cmd" ] ->
                                False

                            [ "Platform", "Sub" ] ->
                                False

                            [ "Process" ] ->
                                False

                            [ "Task" ] ->
                                False

                            [ "Json", "Decode" ] ->
                                False

                            [ "Json", "Encode" ] ->
                                False

                            [ "Parser" ] ->
                                False

                            [ "Parser", "Advanced" ] ->
                                False

                            [ "Regex" ] ->
                                False

                            [ "File" ] ->
                                False

                            [ "Bytes" ] ->
                                False

                            [ "Bytes", "Encode" ] ->
                                False

                            [ "Bytes", "Decode" ] ->
                                False

                            [ "Http" ] ->
                                False

                            [ "VirtualDom" ] ->
                                False

                            [ "Browser" ] ->
                                False

                            [ "Browser", "Events" ] ->
                                False

                            [ "Browser", "Navigation" ] ->
                                False

                            [ "Browser", "Dom" ] ->
                                False

                            -- https://github.com/elm/browser/blob/master/src/Browser/AnimationManager.elm
                            [ "Browser", "AnimationManager" ] ->
                                False

                            -- https://github.com/elm/browser/tree/master/src/Debugger
                            [ "Debugger", "Expando" ] ->
                                False

                            [ "Debugger", "History" ] ->
                                False

                            [ "Debugger", "Main" ] ->
                                False

                            [ "Debugger", "Metadata" ] ->
                                False

                            [ "Debugger", "Overlay" ] ->
                                False

                            [ "Debugger", "Report" ] ->
                                False

                            [ "Html" ] ->
                                False

                            [ "Html", "Attributes" ] ->
                                False

                            [ "Html", "Events" ] ->
                                False

                            [ "Html", "Keyed" ] ->
                                False

                            [ "Html", "Lazy" ] ->
                                False

                            [ "Svg" ] ->
                                False

                            [ "Svg", "Attributes" ] ->
                                False

                            [ "Svg", "Events" ] ->
                                False

                            [ "Svg", "Keyed" ] ->
                                False

                            [ "Svg", "Lazy" ] ->
                                False

                            [ "Time" ] ->
                                False

                            [ "Random" ] ->
                                False

                            [ "Url" ] ->
                                False

                            [ "Url", "Builder" ] ->
                                False

                            [ "Url", "Parser" ] ->
                                False

                            [ "Url", "Parser", "Query" ] ->
                                False

                            [ "Markdown" ] ->
                                False

                            [ "Benchmark" ] ->
                                False

                            [ "WebGL" ] ->
                                False

                            [ "WebGL", "Settings" ] ->
                                False

                            [ "WebGL", "Settings", "Blend" ] ->
                                False

                            [ "WebGL", "Settings", "DepthTest" ] ->
                                False

                            [ "WebGL", "Settings", "StencilTest" ] ->
                                False

                            [ "WebGL", "Texture" ] ->
                                False

                            [ "Math", "Matrix4" ] ->
                                False

                            [ "Math", "Vector2" ] ->
                                False

                            [ "Math", "Vector3" ] ->
                                False

                            [ "Math", "Vector4" ] ->
                                False

                            _ ->
                                True
                    )

        moduleMembers :
            FastDict.Dict
                Elm.Syntax.ModuleName.ModuleName
                { valueOrFunctionOrTypeAliasNames : FastSet.Set String
                , enumTypesExposingVariants :
                    FastDict.Dict String (FastDict.Dict String { valueCount : Int })
                }
        moduleMembers =
            syntaxDeclarationsIncludingOverwrittenOnes
                |> List.filter
                    (\syntaxModule ->
                        -- remove those modules we don't have a replacement for, yet
                        case
                            syntaxModule.moduleDefinition
                                |> Elm.Syntax.Node.value
                                |> moduleHeaderName
                        of
                            [ "Array" ] ->
                                False

                            -- https://github.com/elm/core/blob/1.0.5/src/Elm/JsArray.elm
                            [ "Elm", "JsArray" ] ->
                                False

                            [ "Bitwise" ] ->
                                False

                            [ "Debug" ] ->
                                False

                            [ "Set" ] ->
                                False

                            [ "Platform" ] ->
                                False

                            [ "Platform", "Cmd" ] ->
                                False

                            [ "Platform", "Sub" ] ->
                                False

                            [ "Process" ] ->
                                False

                            [ "Task" ] ->
                                False

                            [ "Json", "Decode" ] ->
                                False

                            [ "Json", "Encode" ] ->
                                False

                            [ "Parser" ] ->
                                False

                            [ "Parser", "Advanced" ] ->
                                False

                            [ "Regex" ] ->
                                False

                            [ "File" ] ->
                                False

                            [ "Bytes" ] ->
                                False

                            [ "Bytes", "Encode" ] ->
                                False

                            [ "Bytes", "Decode" ] ->
                                False

                            [ "Http" ] ->
                                False

                            [ "VirtualDom" ] ->
                                False

                            [ "Browser" ] ->
                                False

                            [ "Browser", "Events" ] ->
                                False

                            [ "Browser", "Navigation" ] ->
                                False

                            [ "Browser", "Dom" ] ->
                                False

                            -- https://github.com/elm/browser/blob/master/src/Browser/AnimationManager.elm
                            [ "Browser", "AnimationManager" ] ->
                                False

                            -- https://github.com/elm/browser/tree/master/src/Debugger
                            [ "Debugger", "Expando" ] ->
                                False

                            [ "Debugger", "History" ] ->
                                False

                            [ "Debugger", "Main" ] ->
                                False

                            [ "Debugger", "Metadata" ] ->
                                False

                            [ "Debugger", "Overlay" ] ->
                                False

                            [ "Debugger", "Report" ] ->
                                False

                            [ "Html" ] ->
                                False

                            [ "Html", "Attributes" ] ->
                                False

                            [ "Html", "Events" ] ->
                                False

                            [ "Html", "Keyed" ] ->
                                False

                            [ "Html", "Lazy" ] ->
                                False

                            [ "Svg" ] ->
                                False

                            [ "Svg", "Attributes" ] ->
                                False

                            [ "Svg", "Events" ] ->
                                False

                            [ "Svg", "Keyed" ] ->
                                False

                            [ "Svg", "Lazy" ] ->
                                False

                            [ "Time" ] ->
                                False

                            [ "Random" ] ->
                                False

                            [ "Url" ] ->
                                False

                            [ "Url", "Builder" ] ->
                                False

                            [ "Url", "Parser" ] ->
                                False

                            [ "Url", "Parser", "Query" ] ->
                                False

                            [ "Markdown" ] ->
                                False

                            [ "Benchmark" ] ->
                                False

                            [ "WebGL" ] ->
                                False

                            [ "WebGL", "Settings" ] ->
                                False

                            [ "WebGL", "Settings", "Blend" ] ->
                                False

                            [ "WebGL", "Settings", "DepthTest" ] ->
                                False

                            [ "WebGL", "Settings", "StencilTest" ] ->
                                False

                            [ "WebGL", "Texture" ] ->
                                False

                            [ "Math", "Matrix4" ] ->
                                False

                            [ "Math", "Vector2" ] ->
                                False

                            [ "Math", "Vector3" ] ->
                                False

                            [ "Math", "Vector4" ] ->
                                False

                            _ ->
                                True
                    )
                |> List.foldl
                    (\syntaxModule acrossModulesSoFar ->
                        acrossModulesSoFar
                            |> FastDict.insert
                                (syntaxModule.moduleDefinition
                                    |> Elm.Syntax.Node.value
                                    |> moduleHeaderName
                                )
                                (syntaxModule.declarations
                                    |> List.foldl
                                        (\(Elm.Syntax.Node.Node _ declaration) membersSoFar ->
                                            case declaration of
                                                Elm.Syntax.Declaration.FunctionDeclaration syntaxValueOrFunctionDeclaration ->
                                                    { valueOrFunctionOrTypeAliasNames =
                                                        membersSoFar.valueOrFunctionOrTypeAliasNames
                                                            |> FastSet.insert
                                                                (syntaxValueOrFunctionDeclaration.declaration
                                                                    |> Elm.Syntax.Node.value
                                                                    |> .name
                                                                    |> Elm.Syntax.Node.value
                                                                )
                                                    , enumTypesExposingVariants =
                                                        membersSoFar.enumTypesExposingVariants
                                                    }

                                                Elm.Syntax.Declaration.CustomTypeDeclaration syntaxEnumTypeDeclaration ->
                                                    { valueOrFunctionOrTypeAliasNames =
                                                        membersSoFar.valueOrFunctionOrTypeAliasNames
                                                    , enumTypesExposingVariants =
                                                        membersSoFar.enumTypesExposingVariants
                                                            |> FastDict.insert
                                                                (syntaxEnumTypeDeclaration.name |> Elm.Syntax.Node.value)
                                                                (syntaxEnumTypeDeclaration.constructors
                                                                    |> List.foldl
                                                                        (\(Elm.Syntax.Node.Node _ variant) variantNamesSoFar ->
                                                                            variantNamesSoFar
                                                                                |> FastDict.insert
                                                                                    (variant.name
                                                                                        |> Elm.Syntax.Node.value
                                                                                    )
                                                                                    { valueCount =
                                                                                        variant.arguments |> List.length
                                                                                    }
                                                                        )
                                                                        FastDict.empty
                                                                )
                                                    }

                                                Elm.Syntax.Declaration.AliasDeclaration typeAlias ->
                                                    { valueOrFunctionOrTypeAliasNames =
                                                        membersSoFar.valueOrFunctionOrTypeAliasNames
                                                            |> FastSet.insert
                                                                (typeAlias.name
                                                                    |> Elm.Syntax.Node.value
                                                                )
                                                    , enumTypesExposingVariants =
                                                        membersSoFar.enumTypesExposingVariants
                                                    }

                                                Elm.Syntax.Declaration.PortDeclaration _ ->
                                                    -- not supported
                                                    membersSoFar

                                                Elm.Syntax.Declaration.InfixDeclaration _ ->
                                                    membersSoFar

                                                Elm.Syntax.Declaration.Destructuring _ _ ->
                                                    -- invalid syntax
                                                    membersSoFar
                                        )
                                        { valueOrFunctionOrTypeAliasNames = FastSet.empty
                                        , enumTypesExposingVariants = FastDict.empty
                                        }
                                )
                    )
                    FastDict.empty

        gleamDeclarationsWithoutExtraRecordTypeAliases :
            { errors : List String
            , declarations :
                { valuesAndFunctions :
                    FastDict.Dict
                        String
                        { parameters : List (Maybe String)
                        , result : GleamExpression
                        , type_ : Maybe GleamType
                        }
                , typeAliases :
                    FastDict.Dict
                        String
                        { parameters : List String
                        , type_ : GleamType
                        }
                , choiceTypes :
                    FastDict.Dict
                        String
                        { parameters : List String
                        , variants : FastDict.Dict String (List GleamType)
                        }
                }
            }
        gleamDeclarationsWithoutExtraRecordTypeAliases =
            syntaxModules
                |> List.foldr
                    (\syntaxModule soFarAcrossModules ->
                        let
                            moduleName : Elm.Syntax.ModuleName.ModuleName
                            moduleName =
                                syntaxModule.moduleDefinition
                                    |> Elm.Syntax.Node.value
                                    |> moduleHeaderName

                            createdModuleContext : ModuleContext
                            createdModuleContext =
                                moduleContextMerge
                                    (syntaxModule.imports |> importsToModuleContext moduleMembers)
                                    (case moduleMembers |> FastDict.get moduleName of
                                        Nothing ->
                                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                                FastDict.empty
                                            , variantLookup = FastDict.empty
                                            }

                                        Just moduleLocalNames ->
                                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                                FastSet.union
                                                    moduleLocalNames.valueOrFunctionOrTypeAliasNames
                                                    (moduleLocalNames.enumTypesExposingVariants
                                                        |> FastDict.foldl
                                                            (\enumTypeName _ soFar ->
                                                                soFar |> FastSet.insert enumTypeName
                                                            )
                                                            FastSet.empty
                                                    )
                                                    |> FastSet.foldl
                                                        (\name soFar ->
                                                            soFar
                                                                |> FastDict.insert ( [], name )
                                                                    moduleName
                                                        )
                                                        FastDict.empty
                                            , variantLookup =
                                                moduleLocalNames.enumTypesExposingVariants
                                                    |> FastDict.foldl
                                                        (\_ variantNames soFarAcrossEnumTypes ->
                                                            variantNames
                                                                |> FastDict.foldl
                                                                    (\name info soFar ->
                                                                        soFar
                                                                            |> FastDict.insert ( [], name )
                                                                                { moduleOrigin = moduleName
                                                                                , valueCount = info.valueCount
                                                                                }
                                                                    )
                                                                    soFarAcrossEnumTypes
                                                        )
                                                        FastDict.empty
                                            }
                                    )
                        in
                        syntaxModule.declarations
                            |> List.foldr
                                (\(Elm.Syntax.Node.Node _ declaration) soFar ->
                                    case declaration of
                                        Elm.Syntax.Declaration.FunctionDeclaration syntaxValueOrFunctionDeclaration ->
                                            case syntaxValueOrFunctionDeclaration |> valueOrFunctionDeclaration createdModuleContext of
                                                Ok gleamValueOrFunctionDeclaration ->
                                                    { errors = soFar.errors
                                                    , declarations =
                                                        { typeAliases = soFar.declarations.typeAliases
                                                        , choiceTypes = soFar.declarations.choiceTypes
                                                        , valuesAndFunctions =
                                                            soFar.declarations.valuesAndFunctions
                                                                |> FastDict.insert
                                                                    ({ moduleOrigin = moduleName
                                                                     , name = gleamValueOrFunctionDeclaration.name
                                                                     }
                                                                        |> lowercaseReferenceToGleamName
                                                                    )
                                                                    { parameters = gleamValueOrFunctionDeclaration.parameters
                                                                    , result = gleamValueOrFunctionDeclaration.result
                                                                    , type_ = gleamValueOrFunctionDeclaration.type_
                                                                    }
                                                        }
                                                    }

                                                Err error ->
                                                    { declarations = soFar.declarations
                                                    , errors = error :: soFar.errors
                                                    }

                                        Elm.Syntax.Declaration.AliasDeclaration syntaxTypeAliasDeclaration ->
                                            case syntaxTypeAliasDeclaration |> typeAliasDeclaration createdModuleContext of
                                                Ok gleamTypeAliasDeclaration ->
                                                    { errors = soFar.errors
                                                    , declarations =
                                                        { valuesAndFunctions = soFar.declarations.valuesAndFunctions
                                                        , choiceTypes = soFar.declarations.choiceTypes
                                                        , typeAliases =
                                                            soFar.declarations.typeAliases
                                                                |> FastDict.insert
                                                                    ({ moduleOrigin = moduleName
                                                                     , name = gleamTypeAliasDeclaration.name
                                                                     }
                                                                        |> uppercaseReferenceToGleamName
                                                                    )
                                                                    { parameters = gleamTypeAliasDeclaration.parameters
                                                                    , type_ = gleamTypeAliasDeclaration.type_
                                                                    }
                                                        }
                                                    }

                                                Err error ->
                                                    { declarations = soFar.declarations
                                                    , errors = error :: soFar.errors
                                                    }

                                        Elm.Syntax.Declaration.CustomTypeDeclaration syntaxEnumTypeDeclaration ->
                                            case syntaxEnumTypeDeclaration.name |> Elm.Syntax.Node.value of
                                                "Maybe" ->
                                                    soFar

                                                _ ->
                                                    case syntaxEnumTypeDeclaration |> enumTypeDeclaration createdModuleContext of
                                                        Ok gleamTypeAliasDeclaration ->
                                                            { errors = soFar.errors
                                                            , declarations =
                                                                { valuesAndFunctions = soFar.declarations.valuesAndFunctions
                                                                , typeAliases = soFar.declarations.typeAliases
                                                                , choiceTypes =
                                                                    soFar.declarations.choiceTypes
                                                                        |> FastDict.insert
                                                                            ({ moduleOrigin = moduleName
                                                                             , name = gleamTypeAliasDeclaration.name
                                                                             }
                                                                                |> uppercaseReferenceToGleamName
                                                                            )
                                                                            { parameters = gleamTypeAliasDeclaration.parameters
                                                                            , variants =
                                                                                gleamTypeAliasDeclaration.variants
                                                                                    |> FastDict.foldl
                                                                                        (\variantName maybeValue variantsSoFar ->
                                                                                            variantsSoFar
                                                                                                |> FastDict.insert
                                                                                                    ({ moduleOrigin = moduleName
                                                                                                     , name = variantName
                                                                                                     }
                                                                                                        |> uppercaseReferenceToGleamName
                                                                                                    )
                                                                                                    maybeValue
                                                                                        )
                                                                                        FastDict.empty
                                                                            }
                                                                }
                                                            }

                                                        Err error ->
                                                            { declarations = soFar.declarations
                                                            , errors = error :: soFar.errors
                                                            }

                                        Elm.Syntax.Declaration.PortDeclaration _ ->
                                            soFar

                                        Elm.Syntax.Declaration.InfixDeclaration _ ->
                                            soFar

                                        Elm.Syntax.Declaration.Destructuring _ _ ->
                                            soFar
                                )
                                soFarAcrossModules
                    )
                    { errors = []
                    , declarations =
                        { valuesAndFunctions = FastDict.empty
                        , typeAliases = FastDict.empty
                        , choiceTypes = FastDict.empty
                        }
                    }

        additionalRecordTypeAliases : FastSet.Set (List String)
        additionalRecordTypeAliases =
            syntaxModules
                |> listMapToFastSetsAndUnify
                    (\syntaxModule ->
                        syntaxModule.declarations
                            |> listMapToFastSetsAndUnify
                                (\(Elm.Syntax.Node.Node _ syntaxDeclaration) ->
                                    case syntaxDeclaration of
                                        Elm.Syntax.Declaration.FunctionDeclaration syntaxValueOrFunctionDeclaration ->
                                            FastSet.union
                                                (case syntaxValueOrFunctionDeclaration.signature of
                                                    Nothing ->
                                                        FastSet.empty

                                                    Just (Elm.Syntax.Node.Node _ signature) ->
                                                        signature.typeAnnotation |> typeContainedRecords
                                                )
                                                (syntaxValueOrFunctionDeclaration
                                                    |> .declaration
                                                    |> Elm.Syntax.Node.value
                                                    |> .expression
                                                    |> expressionContainedRecordsFieldNamesSanitizedForGleam
                                                )

                                        Elm.Syntax.Declaration.AliasDeclaration syntaxTypeAliasDeclaration ->
                                            syntaxTypeAliasDeclaration.typeAnnotation
                                                |> typeContainedRecords

                                        Elm.Syntax.Declaration.CustomTypeDeclaration syntaxChoiceTypeDeclaration ->
                                            syntaxChoiceTypeDeclaration.constructors
                                                |> listMapToFastSetsAndUnify
                                                    (\(Elm.Syntax.Node.Node _ syntaxVariant) ->
                                                        syntaxVariant.arguments
                                                            |> listMapToFastSetsAndUnify
                                                                typeContainedRecords
                                                    )

                                        _ ->
                                            FastSet.empty
                                )
                    )
    in
    { declarations =
        { valuesAndFunctions =
            gleamDeclarationsWithoutExtraRecordTypeAliases.declarations.valuesAndFunctions
                |> FastDict.map
                    (\_ valueOrFunctionInfo ->
                        { type_ = valueOrFunctionInfo.type_
                        , parameters = valueOrFunctionInfo.parameters
                        , result = valueOrFunctionInfo.result
                        }
                    )
        , choiceTypes =
            gleamDeclarationsWithoutExtraRecordTypeAliases.declarations.choiceTypes
                |> FastDict.map
                    (\_ typeAliasInfo ->
                        { parameters = typeAliasInfo.parameters
                        , variants = typeAliasInfo.variants
                        }
                    )
        , recordTypes = additionalRecordTypeAliases
        , typeAliases =
            gleamDeclarationsWithoutExtraRecordTypeAliases.declarations.typeAliases
                |> FastDict.map
                    (\_ typeAliasInfo ->
                        { parameters = typeAliasInfo.parameters
                        , type_ = typeAliasInfo.type_
                        }
                    )
        }
    , errors = gleamDeclarationsWithoutExtraRecordTypeAliases.errors
    }


expressionContainedRecordsFieldNamesSanitizedForGleam :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> FastSet.Set (List String)
expressionContainedRecordsFieldNamesSanitizedForGleam expressionNode =
    -- IGNORE TCO
    case expressionNode of
        Elm.Syntax.Node.Node _ (Elm.Syntax.Expression.RecordExpr fields) ->
            FastSet.singleton
                (fields
                    |> listMapAndToFastSet
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, _ )) ->
                            lowercaseNameSanitizeForGleam fieldName
                        )
                    |> FastSet.toList
                )

        expressionNodeNotRecord ->
            expressionNodeNotRecord
                |> expressionSubs
                |> listMapToFastSetsAndUnify
                    expressionContainedRecordsFieldNamesSanitizedForGleam


{-| All surface-level child [expression](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression)s
-}
expressionSubs :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)
expressionSubs (Elm.Syntax.Node.Node _ syntaxExpression) =
    case syntaxExpression of
        Elm.Syntax.Expression.Application expressions ->
            expressions

        Elm.Syntax.Expression.ListExpr elements ->
            elements

        Elm.Syntax.Expression.RecordExpr fields ->
            List.map (\(Elm.Syntax.Node.Node _ ( _, expr )) -> expr) fields

        Elm.Syntax.Expression.RecordUpdateExpression _ setters ->
            List.map (\(Elm.Syntax.Node.Node _ ( _, expr )) -> expr) setters

        Elm.Syntax.Expression.ParenthesizedExpression expr ->
            [ expr ]

        Elm.Syntax.Expression.OperatorApplication _ direction left right ->
            case direction of
                Elm.Syntax.Infix.Left ->
                    [ left, right ]

                Elm.Syntax.Infix.Right ->
                    [ right, left ]

                Elm.Syntax.Infix.Non ->
                    [ left, right ]

        Elm.Syntax.Expression.IfBlock cond then_ else_ ->
            [ cond, then_, else_ ]

        Elm.Syntax.Expression.LetExpression letIn ->
            List.foldr
                (\declaration soFar ->
                    case Elm.Syntax.Node.value declaration of
                        Elm.Syntax.Expression.LetFunction function ->
                            (function.declaration
                                |> Elm.Syntax.Node.value
                                |> .expression
                            )
                                :: soFar

                        Elm.Syntax.Expression.LetDestructuring _ expr ->
                            expr :: soFar
                )
                [ letIn.expression ]
                letIn.declarations

        Elm.Syntax.Expression.CaseExpression caseOf ->
            caseOf.expression
                :: List.map (\( _, caseExpression ) -> caseExpression) caseOf.cases

        Elm.Syntax.Expression.LambdaExpression lambda ->
            [ lambda.expression ]

        Elm.Syntax.Expression.TupledExpression expressions ->
            expressions

        Elm.Syntax.Expression.Negation expr ->
            [ expr ]

        Elm.Syntax.Expression.RecordAccess expr _ ->
            [ expr ]

        Elm.Syntax.Expression.PrefixOperator _ ->
            []

        Elm.Syntax.Expression.Operator _ ->
            []

        Elm.Syntax.Expression.Integer _ ->
            []

        Elm.Syntax.Expression.Hex _ ->
            []

        Elm.Syntax.Expression.Floatable _ ->
            []

        Elm.Syntax.Expression.Literal _ ->
            []

        Elm.Syntax.Expression.CharLiteral _ ->
            []

        Elm.Syntax.Expression.UnitExpr ->
            []

        Elm.Syntax.Expression.FunctionOrValue _ _ ->
            []

        Elm.Syntax.Expression.RecordAccessFunction _ ->
            []

        Elm.Syntax.Expression.GLSLExpression _ ->
            []


generatedGleamRecordNameForFields : List String -> String
generatedGleamRecordNameForFields recordFields =
    "Generated"
        ++ (recordFields
                |> List.map uppercaseNameSanitizeForGleam
                |> String.join "0"
           )


moduleHeaderName : Elm.Syntax.Module.Module -> Elm.Syntax.ModuleName.ModuleName
moduleHeaderName moduleHeader =
    case moduleHeader of
        Elm.Syntax.Module.NormalModule header ->
            header.moduleName |> Elm.Syntax.Node.value

        Elm.Syntax.Module.PortModule header ->
            header.moduleName |> Elm.Syntax.Node.value

        Elm.Syntax.Module.EffectModule header ->
            header.moduleName |> Elm.Syntax.Node.value


valueOrFunctionDeclaration :
    ModuleContext
    -> Elm.Syntax.Expression.Function
    ->
        Result
            String
            { name : String
            , parameters : List (Maybe String)
            , result : GleamExpression
            , type_ : Maybe GleamType
            }
valueOrFunctionDeclaration context syntaxDeclarationValueOrFunction =
    let
        implementation : Elm.Syntax.Expression.FunctionImplementation
        implementation =
            syntaxDeclarationValueOrFunction.declaration |> Elm.Syntax.Node.value
    in
    resultAndThen2
        (\parameters maybeType ->
            Result.map
                (\result ->
                    let
                        parametersAsNamesOrIgnored : List (Maybe { name : String, pattern : GleamPattern })
                        parametersAsNamesOrIgnored =
                            parameters
                                |> List.indexedMap
                                    (\parameterIndex parameter ->
                                        parameterGeneratedGleamName parameterIndex parameter.pattern
                                            |> Maybe.map
                                                (\name ->
                                                    { name = name, pattern = parameter.pattern }
                                                )
                                    )

                        resultWithRecordDestructurings : GleamExpression
                        resultWithRecordDestructurings =
                            result
                                |> gleamAddRecordFieldDestructuringLetValues
                                    (parameters
                                        |> listMapToFastDictsAndUnify
                                            .recordFieldDestructuringsToAdd
                                    )
                    in
                    { name =
                        implementation.name
                            |> Elm.Syntax.Node.value
                            |> lowercaseNameSanitizeForGleam
                    , type_ = maybeType
                    , parameters =
                        parametersAsNamesOrIgnored
                            |> List.map
                                (\parameter ->
                                    parameter |> Maybe.map .name
                                )
                    , result =
                        case parametersAsNamesOrIgnored |> List.filterMap identity of
                            [] ->
                                resultWithRecordDestructurings

                            parameterToDestructure0 :: parameterToDestructure1Up ->
                                GleamExpressionWithLetDeclarations
                                    { declaration0 =
                                        GleamLetDestructuring
                                            { expression =
                                                GleamExpressionReference
                                                    { moduleOrigin = Nothing
                                                    , name = parameterToDestructure0.name
                                                    }
                                            , pattern = parameterToDestructure0.pattern
                                            }
                                    , declaration1Up =
                                        parameterToDestructure1Up
                                            |> List.map
                                                (\parameterToDestructure ->
                                                    GleamLetDestructuring
                                                        { expression =
                                                            GleamExpressionReference
                                                                { moduleOrigin = Nothing
                                                                , name = parameterToDestructure.name
                                                                }
                                                        , pattern = parameterToDestructure.pattern
                                                        }
                                                )
                                    , result = resultWithRecordDestructurings
                                    }
                    }
                )
                (implementation.expression
                    |> expression
                        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                            context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                        , variantLookup =
                            context.variantLookup
                        , variablesFromWithinDeclarationInScope =
                            parameters
                                |> listMapToFastSetsAndUnify .introducedVariables
                        }
                )
        )
        (implementation.arguments
            |> listMapAndCombineOk
                (\p ->
                    p
                        |> pattern
                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                            , variantLookup = context.variantLookup
                            }
                )
        )
        (case syntaxDeclarationValueOrFunction.signature of
            Nothing ->
                Ok Nothing

            Just (Elm.Syntax.Node.Node _ signature) ->
                Result.map Just
                    (signature.typeAnnotation
                        |> type_
                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                            , variantLookup = context.variantLookup
                            }
                    )
        )


gleamAddRecordFieldDestructuringLetValues :
    FastDict.Dict String { recordVariable : String }
    -> GleamExpression
    -> GleamExpression
gleamAddRecordFieldDestructuringLetValues recordFieldDestructuringsToAdd gleamExpression =
    let
        letDeclarationsToAdd : List GleamLetDeclaration
        letDeclarationsToAdd =
            recordFieldDestructuringsToAdd
                |> FastDict.foldl
                    (\field fieldOrigin soFar ->
                        GleamLetDestructuring
                            { pattern = GleamPatternVariable field
                            , expression =
                                GleamExpressionRecordAccess
                                    { record =
                                        GleamExpressionReference
                                            { moduleOrigin = Nothing
                                            , name = fieldOrigin.recordVariable
                                            }
                                    , field = field
                                    }
                            }
                            :: soFar
                    )
                    []
    in
    case letDeclarationsToAdd of
        [] ->
            gleamExpression

        declaration0 :: declaration1Up ->
            GleamExpressionWithLetDeclarations
                { declaration0 = declaration0
                , declaration1Up = declaration1Up
                , result = gleamExpression
                }


lowercaseNameSanitizeForGleam : String -> String
lowercaseNameSanitizeForGleam lowercaseName =
    let
        lowercaseNameWithValidCharacters : String
        lowercaseNameWithValidCharacters =
            lowercaseName
                |> stringFirstCharToLower
                |> String.toList
                |> List.map
                    (\nameChar ->
                        if nameChar |> Char.isUpper then
                            "_" ++ (nameChar |> Char.toLower |> String.fromChar)

                        else
                            nameChar |> String.fromChar
                    )
                |> String.concat
    in
    if gleamReservedWords |> FastSet.member lowercaseNameWithValidCharacters then
        lowercaseNameWithValidCharacters ++ "1"

    else
        lowercaseNameWithValidCharacters


uppercaseNameSanitizeForGleam : String -> String
uppercaseNameSanitizeForGleam uppercaseName =
    let
        uppercaseNameWithValidCharacters : String
        uppercaseNameWithValidCharacters =
            uppercaseName
                |> String.replace "_" "1"
                |> stringFirstCharToUpper
    in
    if gleamReservedWords |> FastSet.member uppercaseNameWithValidCharacters then
        uppercaseNameWithValidCharacters ++ "1"

    else
        uppercaseNameWithValidCharacters


gleamReservedWords : FastSet.Set String
gleamReservedWords =
    -- https://github.com/gleam-lang/gleam/blob/main/compiler-core/src/parse/token.rs#L67
    FastSet.fromList
        [ "as"
        , "assert"
        , "auto"
        , "case"
        , "const"
        , "delegate"
        , "derive"
        , "echo"
        , "else"
        , "fn"
        , "if"
        , "implement"
        , "import"
        , "let"
        , "macro"
        , "opaque"
        , "panic"
        , "pub"
        , "test"
        , "todo"
        , "type"
        , "use"
        , --
          "Bool"
        , "Float"
        , "Int"
        , "Result"
        , "List"
        , "Ok"
        , "Error"
        , "String"
        , "UtfCodepoint"
        ]


expressionContextAddVariablesInScope :
    FastSet.Set String
    ->
        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                Elm.Syntax.ModuleName.ModuleName
        , variantLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                , valueCount : Int
                }
        , variablesFromWithinDeclarationInScope : FastSet.Set String
        }
    ->
        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                Elm.Syntax.ModuleName.ModuleName
        , variantLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                , valueCount : Int
                }
        , variablesFromWithinDeclarationInScope : FastSet.Set String
        }
expressionContextAddVariablesInScope additionalVariablesInScope context =
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
        context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
    , variantLookup =
        context.variantLookup
    , variablesFromWithinDeclarationInScope =
        FastSet.union
            additionalVariablesInScope
            context.variablesFromWithinDeclarationInScope
    }


expression :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Result String GleamExpression
expression context (Elm.Syntax.Node.Node _ syntaxExpression) =
    -- IGNORE TCO
    case syntaxExpression of
        Elm.Syntax.Expression.UnitExpr ->
            Ok gleamExpressionNil

        Elm.Syntax.Expression.Integer intValue ->
            Ok (GleamExpressionFloat (intValue |> Basics.toFloat))

        Elm.Syntax.Expression.Hex intValue ->
            Ok (GleamExpressionFloat (intValue |> Basics.toFloat))

        Elm.Syntax.Expression.Floatable floatValue ->
            Ok (GleamExpressionFloat floatValue)

        Elm.Syntax.Expression.CharLiteral charValue ->
            Ok (GleamExpressionChar charValue)

        Elm.Syntax.Expression.Literal stringValue ->
            Ok (GleamExpressionString stringValue)

        Elm.Syntax.Expression.RecordAccessFunction fieldName ->
            let
                recordVariableName : String
                recordVariableName =
                    "generated_record"
            in
            Ok
                (GleamExpressionLambda
                    { parameter0 = Just recordVariableName
                    , parameter1Up = []
                    , result =
                        GleamExpressionRecordAccess
                            { record =
                                GleamExpressionReference
                                    { moduleOrigin = Nothing
                                    , name = recordVariableName
                                    }
                            , field =
                                fieldName
                                    |> String.replace "." ""
                                    |> lowercaseNameSanitizeForGleam
                            }
                    }
                )

        Elm.Syntax.Expression.Operator _ ->
            -- invalid syntax
            Err "operator is invalid syntax"

        Elm.Syntax.Expression.PrefixOperator operatorSymbol ->
            Result.map
                (\operationFunctionReference ->
                    GleamExpressionReference operationFunctionReference
                )
                (expressionOperatorToGleamFunctionReference operatorSymbol)

        Elm.Syntax.Expression.GLSLExpression _ ->
            Err "glsl not supported"

        Elm.Syntax.Expression.Application application ->
            case application of
                [] ->
                    Err "application without any parts is invalid"

                [ inParens ] ->
                    -- invalid syntax
                    expression context inParens

                calledNode :: argument0Node :: argument1UpNodes ->
                    Result.map3
                        (\called argument0 argument1Up ->
                            condenseExpressionCall
                                { called = called
                                , argument0 = argument0
                                , argument1Up = argument1Up
                                }
                        )
                        (calledNode |> expression context)
                        (argument0Node |> expression context)
                        (argument1UpNodes
                            |> listMapAndCombineOk
                                (\argument -> argument |> expression context)
                        )

        Elm.Syntax.Expression.OperatorApplication operatorSymbol _ leftNode rightNode ->
            case operatorSymbol of
                "|>" ->
                    Result.map2
                        (\argument called ->
                            condenseExpressionCall
                                { called = called
                                , argument0 = argument
                                , argument1Up = []
                                }
                        )
                        (leftNode |> expression context)
                        (rightNode |> expression context)

                "<|" ->
                    Result.map2
                        (\called argument ->
                            condenseExpressionCall
                                { called = called
                                , argument0 = argument
                                , argument1Up = []
                                }
                        )
                        (leftNode |> expression context)
                        (rightNode |> expression context)

                "++" ->
                    Result.map2
                        (\left right ->
                            if
                                (left |> gleamExpressionIsDefinitelyOfTypeString)
                                    || (right |> gleamExpressionIsDefinitelyOfTypeString)
                            then
                                GleamExpressionCall
                                    { called =
                                        GleamExpressionReference
                                            { moduleOrigin = Just "string"
                                            , name = "append"
                                            }
                                    , argument0 = left
                                    , argument1Up = [ right ]
                                    }

                            else
                                GleamExpressionCall
                                    { called =
                                        GleamExpressionReference
                                            { moduleOrigin = Just "list"
                                            , name = "append"
                                            }
                                    , argument0 = left
                                    , argument1Up = [ right ]
                                    }
                        )
                        (leftNode |> expression context)
                        (rightNode |> expression context)

                otherOperatorSymbol ->
                    Result.map3
                        (\operationFunctionReference left right ->
                            GleamExpressionCall
                                { called =
                                    GleamExpressionReference operationFunctionReference
                                , argument0 = left
                                , argument1Up = [ right ]
                                }
                        )
                        (expressionOperatorToGleamFunctionReference otherOperatorSymbol)
                        (leftNode |> expression context)
                        (rightNode |> expression context)

        Elm.Syntax.Expression.FunctionOrValue qualification name ->
            let
                asVariableFromWithinDeclaration : Maybe String
                asVariableFromWithinDeclaration =
                    case qualification of
                        _ :: _ ->
                            Nothing

                        [] ->
                            let
                                gleamName : String
                                gleamName =
                                    name |> lowercaseNameSanitizeForGleam
                            in
                            if
                                context.variablesFromWithinDeclarationInScope
                                    |> FastSet.member gleamName
                            then
                                Just gleamName

                            else
                                Nothing
            in
            case asVariableFromWithinDeclaration of
                Just variableFromWithinDeclaration ->
                    Ok
                        (GleamExpressionReference
                            { moduleOrigin = Nothing
                            , name = variableFromWithinDeclaration
                            }
                        )

                Nothing ->
                    case context.variantLookup |> FastDict.get ( qualification, name ) of
                        Just variantInfo ->
                            let
                                reference : { moduleOrigin : Maybe String, name : String }
                                reference =
                                    case { moduleOrigin = variantInfo.moduleOrigin, name = name } |> referenceToCoreGleam of
                                        Just gleamReference ->
                                            gleamReference

                                        Nothing ->
                                            { moduleOrigin = Nothing
                                            , name =
                                                uppercaseReferenceToGleamName
                                                    { moduleOrigin = variantInfo.moduleOrigin
                                                    , name = name
                                                    }
                                            }
                            in
                            Ok
                                (case variantInfo.valueCount of
                                    0 ->
                                        GleamExpressionReference reference

                                    1 ->
                                        GleamExpressionReference reference

                                    valueCountAtLeast2 ->
                                        let
                                            generatedValueVariableReference : Int -> GleamExpression
                                            generatedValueVariableReference valueIndex =
                                                GleamExpressionReference
                                                    { moduleOrigin = Nothing
                                                    , name =
                                                        "generated_"
                                                            ++ (valueIndex |> String.fromInt)
                                                    }

                                            generatedValuePattern : Int -> Maybe String
                                            generatedValuePattern valueIndex =
                                                Just
                                                    ("generated_"
                                                        ++ (valueIndex |> String.fromInt)
                                                    )
                                        in
                                        GleamExpressionLambda
                                            { parameter0 = generatedValuePattern 0
                                            , parameter1Up =
                                                generatedValuePattern 1
                                                    :: (List.range 2 (valueCountAtLeast2 - 1)
                                                            |> List.map generatedValuePattern
                                                       )
                                            , result =
                                                GleamExpressionCall
                                                    { called = GleamExpressionReference reference
                                                    , argument0 =
                                                        generatedValueVariableReference 0
                                                    , argument1Up =
                                                        generatedValueVariableReference 1
                                                            :: (List.range 2 (valueCountAtLeast2 - 1)
                                                                    |> List.map generatedValueVariableReference
                                                               )
                                                    }
                                            }
                                )

                        -- not a variant
                        Nothing ->
                            case context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup |> FastDict.get ( qualification, name ) of
                                Just moduleOrigin ->
                                    Ok
                                        (case { moduleOrigin = moduleOrigin, name = name } |> referenceToCoreGleam of
                                            Just gleamReference ->
                                                GleamExpressionReference gleamReference

                                            Nothing ->
                                                -- TODO should be redundant because variant check
                                                if name |> stringFirstCharIsUpper then
                                                    GleamExpressionReference
                                                        { moduleOrigin = Nothing
                                                        , name =
                                                            uppercaseReferenceToGleamName
                                                                { moduleOrigin = moduleOrigin
                                                                , name = name
                                                                }
                                                        }

                                                else
                                                    GleamExpressionReferenceModuleLevelValueAsLazyFn
                                                        { moduleOrigin = Nothing
                                                        , name =
                                                            lowercaseReferenceToGleamName
                                                                { moduleOrigin = moduleOrigin
                                                                , name = name
                                                                }
                                                        }
                                        )

                                -- not a reference that was declared in elm
                                Nothing ->
                                    case qualification of
                                        qualificationPart0 :: qualificationPart1Up ->
                                            Err
                                                ("could not find module origin of the qualified reference "
                                                    ++ (((qualificationPart0 :: qualificationPart1Up) |> String.join ".")
                                                            ++ "."
                                                            ++ name
                                                       )
                                                )

                                        [] ->
                                            -- TODO convert to error
                                            Ok
                                                (GleamExpressionReference
                                                    { moduleOrigin = Nothing
                                                    , name = name |> lowercaseNameSanitizeForGleam
                                                    }
                                                )

        Elm.Syntax.Expression.IfBlock conditionNode onTrueNode onFalseNode ->
            Result.map3
                (\condition onTrue onFalse ->
                    GleamExpressionCase
                        { matched = condition
                        , case0 = { pattern = gleamPatternTrue, result = onTrue }
                        , case1Up = [ { pattern = gleamPatternFalse, result = onFalse } ]
                        }
                )
                (conditionNode |> expression context)
                (onTrueNode |> expression context)
                (onFalseNode |> expression context)

        Elm.Syntax.Expression.ParenthesizedExpression inParens ->
            inParens |> expression context

        Elm.Syntax.Expression.Negation inNegationNode ->
            Result.map
                (\inNegation ->
                    GleamExpressionCall
                        { called =
                            GleamExpressionReference
                                { moduleOrigin = Just "float", name = "negate" }
                        , argument0 = inNegation
                        , argument1Up = []
                        }
                )
                (inNegationNode |> expression context)

        Elm.Syntax.Expression.RecordAccess recordNode (Elm.Syntax.Node.Node _ fieldName) ->
            Result.map
                (\record ->
                    GleamExpressionRecordAccess
                        { record = record
                        , field =
                            fieldName
                                |> String.replace "." ""
                                |> lowercaseNameSanitizeForGleam
                        }
                )
                (recordNode |> expression context)

        Elm.Syntax.Expression.TupledExpression parts ->
            case parts of
                [] ->
                    -- invalid syntax
                    -- should be handled by Elm.Syntax.Expression.UnitExpr
                    Ok gleamExpressionNil

                [ inParens ] ->
                    -- invalid syntax
                    -- should be handled by Elm.Syntax.Expression.ParenthesizedExpression
                    expression context inParens

                [ part0Node, part1Node ] ->
                    Result.map2
                        (\part0 part1 ->
                            GleamExpressionTuple
                                { part0 = part0
                                , part1 = part1
                                , part2Up = []
                                }
                        )
                        (part0Node |> expression context)
                        (part1Node |> expression context)

                [ part0Node, part1Node, part2Node ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            GleamExpressionTuple
                                { part0 = part0
                                , part1 = part1
                                , part2Up = [ part2 ]
                                }
                        )
                        (part0Node |> expression context)
                        (part1Node |> expression context)
                        (part2Node |> expression context)

                _ :: _ :: _ :: _ :: _ ->
                    Err "too many tuple parts"

        Elm.Syntax.Expression.ListExpr elementNodes ->
            Result.map (\elements -> GleamExpressionList elements)
                (elementNodes
                    |> listMapAndCombineOk
                        (\element -> element |> expression context)
                )

        Elm.Syntax.Expression.RecordExpr fieldNodes ->
            Result.map (\fields -> GleamExpressionRecord fields)
                (fieldNodes
                    |> listMapAndCombineOk
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, fieldValueNode )) ->
                            Result.map
                                (\fieldValue ->
                                    ( fieldName
                                        |> lowercaseNameSanitizeForGleam
                                    , fieldValue
                                    )
                                )
                                (fieldValueNode |> expression context)
                        )
                    |> Result.map FastDict.fromList
                )

        Elm.Syntax.Expression.RecordUpdateExpression _ _ ->
            Err "record update not supported"

        Elm.Syntax.Expression.LambdaExpression lambda ->
            case lambda.args of
                [] ->
                    Err "lambda without parameters is invalid syntax"

                parameter0Node :: parameter1UpNodes ->
                    resultAndThen2
                        (\parameter0 parameter1Up ->
                            Result.map
                                (\result ->
                                    let
                                        parameter0AsNameOrIgnored : Maybe { name : String, pattern : GleamPattern }
                                        parameter0AsNameOrIgnored =
                                            parameterGeneratedGleamName 0 parameter0.pattern
                                                |> Maybe.map
                                                    (\name ->
                                                        { name = name, pattern = parameter0.pattern }
                                                    )

                                        parameter1UpAsNamesOrIgnored : List (Maybe { name : String, pattern : GleamPattern })
                                        parameter1UpAsNamesOrIgnored =
                                            parameter1Up
                                                |> List.indexedMap
                                                    (\parameterIndexFrom1 parameter ->
                                                        parameterGeneratedGleamName (1 + parameterIndexFrom1) parameter.pattern
                                                            |> Maybe.map
                                                                (\name ->
                                                                    { name = name, pattern = parameter.pattern }
                                                                )
                                                    )

                                        resultWithRecordDestructurings : GleamExpression
                                        resultWithRecordDestructurings =
                                            result
                                                |> gleamAddRecordFieldDestructuringLetValues
                                                    (FastDict.union
                                                        parameter0.recordFieldDestructuringsToAdd
                                                        (parameter1Up
                                                            |> listMapToFastDictsAndUnify
                                                                .recordFieldDestructuringsToAdd
                                                        )
                                                    )
                                    in
                                    GleamExpressionLambda
                                        { parameter0 =
                                            parameter0AsNameOrIgnored |> Maybe.map .name
                                        , parameter1Up =
                                            parameter1UpAsNamesOrIgnored
                                                |> List.map
                                                    (\parameter ->
                                                        parameter |> Maybe.map .name
                                                    )
                                        , result =
                                            case
                                                parameter0AsNameOrIgnored
                                                    :: parameter1UpAsNamesOrIgnored
                                                    |> List.filterMap identity
                                            of
                                                [] ->
                                                    resultWithRecordDestructurings

                                                parameterToDestructure0 :: parameterToDestructure1Up ->
                                                    GleamExpressionWithLetDeclarations
                                                        { declaration0 =
                                                            GleamLetDestructuring
                                                                { expression =
                                                                    GleamExpressionReference
                                                                        { moduleOrigin = Nothing
                                                                        , name = parameterToDestructure0.name
                                                                        }
                                                                , pattern = parameterToDestructure0.pattern
                                                                }
                                                        , declaration1Up =
                                                            parameterToDestructure1Up
                                                                |> List.map
                                                                    (\parameterToDestructure ->
                                                                        GleamLetDestructuring
                                                                            { expression =
                                                                                GleamExpressionReference
                                                                                    { moduleOrigin = Nothing
                                                                                    , name = parameterToDestructure.name
                                                                                    }
                                                                            , pattern = parameterToDestructure.pattern
                                                                            }
                                                                    )
                                                        , result = resultWithRecordDestructurings
                                                        }
                                        }
                                )
                                (lambda.expression
                                    |> expression
                                        (context
                                            |> expressionContextAddVariablesInScope
                                                (FastSet.union
                                                    parameter0.introducedVariables
                                                    (parameter1Up
                                                        |> listMapToFastSetsAndUnify .introducedVariables
                                                    )
                                                )
                                        )
                                )
                        )
                        (parameter0Node
                            |> pattern
                                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                    context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                                , variantLookup = context.variantLookup
                                }
                        )
                        (parameter1UpNodes
                            |> listMapAndCombineOk
                                (\parameter ->
                                    parameter
                                        |> pattern
                                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                                context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                                            , variantLookup = context.variantLookup
                                            }
                                )
                        )

        Elm.Syntax.Expression.CaseExpression caseOf ->
            case caseOf.cases of
                [] ->
                    Err "case-of without cases invalid syntax"

                case0Node :: case1Node ->
                    Result.map3
                        (\matched case0 case1Up ->
                            GleamExpressionCase
                                { matched = matched
                                , case0 = case0
                                , case1Up = case1Up
                                }
                        )
                        (caseOf.expression |> expression context)
                        (case0Node |> case_ context)
                        (case1Node
                            |> listMapAndCombineOk
                                (\parameter ->
                                    parameter |> case_ context
                                )
                        )

        Elm.Syntax.Expression.LetExpression letIn ->
            case letIn.declarations of
                [] ->
                    Err "let-in without declarations is invalid syntax"

                declaration0Node :: declaration1UpNode ->
                    let
                        variablesForWholeLetIn : FastSet.Set String
                        variablesForWholeLetIn =
                            (declaration0Node :: declaration1UpNode)
                                |> listMapToFastSetsAndUnify
                                    (\(Elm.Syntax.Node.Node _ syntaxLetDeclaration) ->
                                        case syntaxLetDeclaration of
                                            Elm.Syntax.Expression.LetFunction letFunction ->
                                                FastSet.singleton
                                                    (letFunction.declaration
                                                        |> Elm.Syntax.Node.value
                                                        |> .name
                                                        |> Elm.Syntax.Node.value
                                                        |> lowercaseNameSanitizeForGleam
                                                    )

                                            Elm.Syntax.Expression.LetDestructuring patternNode _ ->
                                                patternNode
                                                    |> patternBindings
                                                    |> listMapAndToFastSet
                                                        lowercaseNameSanitizeForGleam
                                    )
                    in
                    Result.map3
                        (\declaration0 declaration1Up result ->
                            GleamExpressionWithLetDeclarations
                                { declaration0 = declaration0.main
                                , declaration1Up =
                                    declaration1Up
                                        |> List.map .main
                                , result = result
                                }
                                |> gleamAddRecordFieldDestructuringLetValues
                                    (FastDict.union
                                        declaration0.recordFieldDestructuringsToAdd
                                        (declaration1Up
                                            |> listMapToFastDictsAndUnify
                                                .recordFieldDestructuringsToAdd
                                        )
                                    )
                        )
                        (declaration0Node
                            |> letDeclaration
                                (context
                                    |> expressionContextAddVariablesInScope
                                        variablesForWholeLetIn
                                )
                        )
                        (declaration1UpNode
                            |> listMapAndCombineOk
                                (\letDecl ->
                                    letDecl
                                        |> letDeclaration
                                            (context
                                                |> expressionContextAddVariablesInScope
                                                    variablesForWholeLetIn
                                            )
                                )
                        )
                        (letIn.expression
                            |> expression
                                (context
                                    |> expressionContextAddVariablesInScope
                                        variablesForWholeLetIn
                                )
                        )


gleamExpressionNil : GleamExpression
gleamExpressionNil =
    GleamExpressionReference
        { moduleOrigin = Nothing
        , name = "Nil"
        }


gleamPatternTrue : GleamPattern
gleamPatternTrue =
    GleamPatternVariant
        { moduleOrigin = Nothing
        , name = "True"
        , values = []
        }


gleamPatternFalse : GleamPattern
gleamPatternFalse =
    GleamPatternVariant
        { moduleOrigin = Nothing
        , name = "False"
        , values = []
        }


{-| Recursively find all introduced variables
in the [pattern](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Pattern)
(like `a` and `b` in `( Just a, { b } )`)
-}
patternBindings : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern -> List String
patternBindings (Elm.Syntax.Node.Node _ syntaxPattern) =
    -- IGNORE TCO
    case syntaxPattern of
        Elm.Syntax.Pattern.VarPattern name ->
            [ name ]

        Elm.Syntax.Pattern.AsPattern afterAsPattern (Elm.Syntax.Node.Node _ name) ->
            name :: (afterAsPattern |> patternBindings)

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            inParens |> patternBindings

        Elm.Syntax.Pattern.ListPattern patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.TuplePattern patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.RecordPattern fields ->
            fields |> List.map Elm.Syntax.Node.value

        Elm.Syntax.Pattern.NamedPattern _ patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            (tailPattern |> patternBindings) ++ (headPattern |> patternBindings)

        Elm.Syntax.Pattern.AllPattern ->
            []

        Elm.Syntax.Pattern.UnitPattern ->
            []

        Elm.Syntax.Pattern.CharPattern _ ->
            []

        Elm.Syntax.Pattern.StringPattern _ ->
            []

        Elm.Syntax.Pattern.IntPattern _ ->
            []

        Elm.Syntax.Pattern.HexPattern _ ->
            []

        Elm.Syntax.Pattern.FloatPattern _ ->
            []


resultAndThen2 :
    (a -> b -> Result error c)
    -> Result error a
    -> Result error b
    -> Result error c
resultAndThen2 abToResult aResult bResult =
    case aResult of
        Err error ->
            Err error

        Ok a ->
            case bResult of
                Err error ->
                    Err error

                Ok b ->
                    abToResult a b


listMapToFastSetsAndUnify :
    (listElement -> FastSet.Set comparableFastSetElement)
    -> List listElement
    -> FastSet.Set comparableFastSetElement
listMapToFastSetsAndUnify elementToSet list =
    list
        |> List.foldl
            (\element soFar ->
                FastSet.union
                    (element |> elementToSet)
                    soFar
            )
            FastSet.empty


listMapToFastDictsAndUnify :
    (listElement -> FastDict.Dict comparableKey value)
    -> List listElement
    -> FastDict.Dict comparableKey value
listMapToFastDictsAndUnify elementToSet list =
    list
        |> List.foldl
            (\element soFar ->
                FastDict.union
                    (element |> elementToSet)
                    soFar
            )
            FastDict.empty


listMapAndToFastSet :
    (a -> comparable)
    -> List a
    -> FastSet.Set comparable
listMapAndToFastSet elementToSetElement list =
    list
        |> List.foldl
            (\element soFar ->
                soFar
                    |> FastSet.insert
                        (element |> elementToSetElement)
            )
            FastSet.empty


condenseExpressionCall :
    { called : GleamExpression
    , argument0 : GleamExpression
    , argument1Up : List GleamExpression
    }
    -> GleamExpression
condenseExpressionCall call =
    case call.called of
        GleamExpressionCall calledCall ->
            condenseExpressionCall
                { called = calledCall.called
                , argument0 = calledCall.argument0
                , argument1Up =
                    calledCall.argument1Up
                        ++ (call.argument0 :: call.argument1Up)
                }

        GleamExpressionLambda calledLambda ->
            case ( calledLambda.parameter0, calledLambda.result ) of
                ( Just "generated_record", GleamExpressionRecordAccess recordAccess ) ->
                    case call.argument1Up of
                        [] ->
                            GleamExpressionRecordAccess
                                { record = call.argument0
                                , field = recordAccess.field
                                }

                        argument1 :: argument2Up ->
                            GleamExpressionCall
                                { called =
                                    GleamExpressionRecordAccess
                                        { record = call.argument0
                                        , field = recordAccess.field
                                        }
                                , argument0 = argument1
                                , argument1Up = argument2Up
                                }

                ( Just "generated_0", GleamExpressionCall variantCall ) ->
                    GleamExpressionCall
                        { called = variantCall.called
                        , argument0 = call.argument0
                        , argument1Up = call.argument1Up
                        }

                _ ->
                    GleamExpressionCall
                        { called = GleamExpressionLambda calledLambda
                        , argument0 = call.argument0
                        , argument1Up = call.argument1Up
                        }

        calledNotCall ->
            GleamExpressionCall
                { called = calledNotCall
                , argument0 = call.argument0
                , argument1Up = call.argument1Up
                }


gleamExpressionIsDefinitelyOfTypeString : GleamExpression -> Bool
gleamExpressionIsDefinitelyOfTypeString gleamExpression =
    case gleamExpression of
        GleamExpressionString _ ->
            True

        GleamExpressionCall call ->
            call.called
                == GleamExpressionReference { moduleOrigin = Just "string", name = "append" }
                && ((call.argument1Up |> List.length) == 1)

        GleamExpressionChar _ ->
            False

        GleamExpressionFloat _ ->
            False

        GleamExpressionReference _ ->
            False

        GleamExpressionRecordAccess _ ->
            False

        GleamExpressionTuple _ ->
            False

        GleamExpressionList _ ->
            False

        GleamExpressionRecord _ ->
            False

        GleamExpressionLambda _ ->
            False

        GleamExpressionCase _ ->
            False

        GleamExpressionWithLetDeclarations _ ->
            False

        GleamExpressionReferenceModuleLevelValueAsLazyFn _ ->
            False


case_ :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Expression.Case
    ->
        Result
            String
            { pattern : GleamPattern, result : GleamExpression }
case_ context ( patternNode, resultNode ) =
    Result.andThen
        (\casePattern ->
            Result.map
                (\result ->
                    { pattern = casePattern.pattern
                    , result =
                        result
                            |> gleamAddRecordFieldDestructuringLetValues
                                casePattern.recordFieldDestructuringsToAdd
                    }
                )
                (resultNode
                    |> expression
                        (context
                            |> expressionContextAddVariablesInScope
                                casePattern.introducedVariables
                        )
                )
        )
        (patternNode
            |> pattern
                { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                    context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                , variantLookup = context.variantLookup
                }
        )


letDeclaration :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration
    ->
        Result
            String
            { main : GleamLetDeclaration
            , recordFieldDestructuringsToAdd :
                FastDict.Dict String { recordVariable : String }
            }
letDeclaration context (Elm.Syntax.Node.Node _ syntaxLetDeclaration) =
    case syntaxLetDeclaration of
        Elm.Syntax.Expression.LetDestructuring destructuringPatternNode destructuringExpressionNode ->
            Result.map2
                (\destructuringPattern destructuringExpression ->
                    { main =
                        GleamLetDestructuring
                            { pattern = destructuringPattern.pattern
                            , expression = destructuringExpression
                            }
                    , recordFieldDestructuringsToAdd = destructuringPattern.recordFieldDestructuringsToAdd
                    }
                )
                (destructuringPatternNode
                    |> pattern
                        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                            context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                        , variantLookup = context.variantLookup
                        }
                )
                (destructuringExpressionNode |> expression context)

        Elm.Syntax.Expression.LetFunction letValueOrFunction ->
            Result.map
                (\gleamLetDeclarationValueOrFunction ->
                    { main =
                        GleamLetDeclarationValueOrFunction
                            gleamLetDeclarationValueOrFunction
                    , recordFieldDestructuringsToAdd = FastDict.empty
                    }
                )
                (letValueOrFunction
                    |> letValueOrFunctionDeclaration context
                )


letValueOrFunctionDeclaration :
    { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Expression.Function
    ->
        Result
            String
            { name : String
            , result : GleamExpression
            , type_ : Maybe GleamType
            }
letValueOrFunctionDeclaration context syntaxDeclarationValueOrFunction =
    let
        implementation : Elm.Syntax.Expression.FunctionImplementation
        implementation =
            syntaxDeclarationValueOrFunction.declaration |> Elm.Syntax.Node.value
    in
    resultAndThen2
        (\parameters maybeType ->
            Result.map
                (\result ->
                    let
                        parametersAsNamesOrIgnored : List (Maybe { name : String, pattern : GleamPattern })
                        parametersAsNamesOrIgnored =
                            parameters
                                |> List.indexedMap
                                    (\parameterIndex parameter ->
                                        parameterGeneratedGleamName parameterIndex parameter.pattern
                                            |> Maybe.map
                                                (\name ->
                                                    { name = name, pattern = parameter.pattern }
                                                )
                                    )

                        resultWithRecordDestructurings : GleamExpression
                        resultWithRecordDestructurings =
                            result
                                |> gleamAddRecordFieldDestructuringLetValues
                                    (parameters
                                        |> listMapToFastDictsAndUnify
                                            .recordFieldDestructuringsToAdd
                                    )

                        resultWithAllPatternDestructures : GleamExpression
                        resultWithAllPatternDestructures =
                            case parametersAsNamesOrIgnored |> List.filterMap identity of
                                [] ->
                                    resultWithRecordDestructurings

                                parameterToDestructure0 :: parameterToDestructure1Up ->
                                    GleamExpressionWithLetDeclarations
                                        { declaration0 =
                                            GleamLetDestructuring
                                                { expression =
                                                    GleamExpressionReference
                                                        { moduleOrigin = Nothing
                                                        , name = parameterToDestructure0.name
                                                        }
                                                , pattern = parameterToDestructure0.pattern
                                                }
                                        , declaration1Up =
                                            parameterToDestructure1Up
                                                |> List.map
                                                    (\parameterToDestructure ->
                                                        GleamLetDestructuring
                                                            { expression =
                                                                GleamExpressionReference
                                                                    { moduleOrigin = Nothing
                                                                    , name = parameterToDestructure.name
                                                                    }
                                                            , pattern = parameterToDestructure.pattern
                                                            }
                                                    )
                                        , result = resultWithRecordDestructurings
                                        }
                    in
                    { name =
                        implementation.name
                            |> Elm.Syntax.Node.value
                            |> lowercaseNameSanitizeForGleam
                    , type_ = maybeType
                    , result =
                        case parametersAsNamesOrIgnored of
                            [] ->
                                resultWithAllPatternDestructures

                            parameter0 :: parameter1Up ->
                                GleamExpressionLambda
                                    { parameter0 = parameter0 |> Maybe.map .name
                                    , parameter1Up =
                                        parameter1Up
                                            |> List.map
                                                (\parameter ->
                                                    parameter |> Maybe.map .name
                                                )
                                    , result = resultWithAllPatternDestructures
                                    }
                    }
                )
                (implementation.expression
                    |> expression
                        { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                            context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                        , variantLookup =
                            context.variantLookup
                        , variablesFromWithinDeclarationInScope =
                            FastSet.union
                                (parameters
                                    |> listMapToFastSetsAndUnify .introducedVariables
                                )
                                context.variablesFromWithinDeclarationInScope
                        }
                )
        )
        (implementation.arguments
            |> listMapAndCombineOk
                (\p ->
                    p
                        |> pattern
                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                            , variantLookup = context.variantLookup
                            }
                )
        )
        (case syntaxDeclarationValueOrFunction.signature of
            Nothing ->
                Ok Nothing

            Just (Elm.Syntax.Node.Node _ signature) ->
                Result.map Just
                    (signature.typeAnnotation
                        |> type_
                            { valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup =
                                context.valueAndFunctionAndTypeAliasAndEnumTypeModuleOriginLookup
                            , variantLookup = context.variantLookup
                            }
                    )
        )


expressionOperatorToGleamFunctionReference :
    String
    -> Result String { moduleOrigin : Maybe String, name : String }
expressionOperatorToGleamFunctionReference operatorSymbol =
    case operatorSymbol of
        "+" ->
            Ok { moduleOrigin = Just "float", name = "add" }

        "-" ->
            Ok { moduleOrigin = Just "float", name = "subtract" }

        "*" ->
            Ok { moduleOrigin = Just "float", name = "multiply" }

        "/" ->
            Ok { moduleOrigin = Nothing, name = "basics_fdiv" }

        "//" ->
            Ok { moduleOrigin = Nothing, name = "basics_idiv" }

        "^" ->
            Ok { moduleOrigin = Just "float", name = "power" }

        "==" ->
            Ok { moduleOrigin = Nothing, name = "basics_eq" }

        "/=" ->
            Ok { moduleOrigin = Nothing, name = "basics_neq" }

        "||" ->
            Ok { moduleOrigin = Just "bool", name = "or" }

        "&&" ->
            Ok { moduleOrigin = Just "bool", name = "and" }

        "<" ->
            Ok { moduleOrigin = Nothing, name = "basics_lt" }

        ">" ->
            Ok { moduleOrigin = Nothing, name = "basics_gt" }

        "<=" ->
            Ok { moduleOrigin = Nothing, name = "basics_le" }

        ">=" ->
            Ok { moduleOrigin = Nothing, name = "basics_ge" }

        "::" ->
            Ok { moduleOrigin = Nothing, name = "list_cons" }

        "++" ->
            Ok { moduleOrigin = Just "list", name = "append" }

        unknownOrUnsupportedOperator ->
            Err ("unknown/unsupported operator " ++ unknownOrUnsupportedOperator)


{-| Print a gleam value/function declaration
-}
printGleamLetValueOrFunctionDeclaration :
    { name : String
    , result : GleamExpression
    , type_ : Maybe GleamType
    }
    -> Print
printGleamLetValueOrFunctionDeclaration gleamValueOrFunctionDeclaration =
    let
        resultPrint : Print
        resultPrint =
            printGleamExpressionParenthesizedIfWithLetDeclarations
                gleamValueOrFunctionDeclaration.result
    in
    Print.exactly ("let " ++ gleamValueOrFunctionDeclaration.name)
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                ((case gleamValueOrFunctionDeclaration.type_ of
                    Nothing ->
                        Print.empty

                    Just declaredType ->
                        let
                            typePrint : Print
                            typePrint =
                                printGleamTypeNotParenthesized declaredType

                            fullLineSpread : Print.LineSpread
                            fullLineSpread =
                                typePrint |> Print.lineSpread
                        in
                        Print.exactly ":"
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented fullLineSpread)
                            |> Print.followedBy typePrint
                 )
                    |> Print.followedBy
                        (Print.exactly " =")
                    |> Print.followedBy
                        Print.linebreakIndented
                    |> Print.followedBy resultPrint
                )
            )


qualifiedGleamReferenceToString :
    { moduleOrigin : Maybe String
    , name : String
    }
    -> String
qualifiedGleamReferenceToString reference =
    case reference.moduleOrigin of
        Nothing ->
            reference.name

        Just moduleOrigin ->
            moduleOrigin
                ++ "."
                ++ reference.name


printGleamExpressionParenthesizedIfSpaceSeparated : GleamExpression -> Print
printGleamExpressionParenthesizedIfSpaceSeparated gleamExpression =
    if gleamExpression |> gleamExpressionIsSpaceSeparated then
        printParenthesized
            { opening = "{"
            , closing = "}"
            , inner = printGleamExpressionNotParenthesized gleamExpression
            }

    else
        printGleamExpressionNotParenthesized gleamExpression


printGleamExpressionParenthesizedIfWithLetDeclarations : GleamExpression -> Print
printGleamExpressionParenthesizedIfWithLetDeclarations gleamExpression =
    case gleamExpression of
        GleamExpressionWithLetDeclarations gleamExpressionWithLetDeclarations ->
            printParenthesized
                { opening = "{"
                , closing = "}"
                , inner =
                    printGleamExpressionWithLetDeclarations
                        gleamExpressionWithLetDeclarations
                }

        gleamExpressionNotWithLetDeclarations ->
            printGleamExpressionNotParenthesized gleamExpressionNotWithLetDeclarations


gleamExpressionIsSpaceSeparated : GleamExpression -> Bool
gleamExpressionIsSpaceSeparated gleamExpression =
    case gleamExpression of
        GleamExpressionChar _ ->
            False

        GleamExpressionFloat _ ->
            False

        GleamExpressionString _ ->
            False

        GleamExpressionReference _ ->
            False

        GleamExpressionReferenceModuleLevelValueAsLazyFn _ ->
            False

        GleamExpressionRecordAccess _ ->
            False

        GleamExpressionTuple _ ->
            False

        GleamExpressionList _ ->
            False

        GleamExpressionRecord _ ->
            False

        GleamExpressionCall _ ->
            False

        GleamExpressionLambda _ ->
            True

        GleamExpressionCase _ ->
            True

        GleamExpressionWithLetDeclarations _ ->
            True


{-| Print a [`GleamExpression`](#GleamExpression)
-}
printGleamExpressionNotParenthesized : GleamExpression -> Print
printGleamExpressionNotParenthesized gleamExpression =
    -- IGNORE TCO
    case gleamExpression of
        GleamExpressionCall call ->
            printGleamExpressionCall call

        GleamExpressionReference reference ->
            Print.exactly
                (reference |> qualifiedGleamReferenceToString)

        GleamExpressionReferenceModuleLevelValueAsLazyFn reference ->
            Print.exactly
                ((reference |> qualifiedGleamReferenceToString)
                    ++ "()"
                )

        GleamExpressionChar charValue ->
            Print.exactly (charLiteral charValue)

        GleamExpressionFloat float ->
            Print.exactly (gleamNumberLiteralToString float)

        GleamExpressionString string ->
            printGleamString string

        GleamExpressionTuple parts ->
            printGleamExpressionTuple parts

        GleamExpressionWithLetDeclarations expressionWithLetDeclarations ->
            printGleamExpressionWithLetDeclarations expressionWithLetDeclarations

        GleamExpressionCase syntaxWhenIs ->
            printGleamExpressionCase syntaxWhenIs

        GleamExpressionLambda syntaxLambda ->
            printGleamExpressionLambda syntaxLambda

        GleamExpressionRecord fields ->
            printGleamExpressionRecord fields

        GleamExpressionList elements ->
            printGleamExpressionList elements

        GleamExpressionRecordAccess syntaxRecordAccess ->
            printGleamExpressionParenthesizedIfSpaceSeparated
                syntaxRecordAccess.record
                |> Print.followedBy
                    (Print.exactly
                        ("." ++ syntaxRecordAccess.field)
                    )


printGleamExpressionTuple :
    { part0 : GleamExpression
    , part1 : GleamExpression
    , part2Up : List GleamExpression
    }
    -> Print
printGleamExpressionTuple parts =
    let
        part0Print : Print
        part0Print =
            printGleamExpressionNotParenthesized
                parts.part0

        part1Print : Print
        part1Print =
            printGleamExpressionNotParenthesized
                parts.part1

        part2UpPrints : List Print
        part2UpPrints =
            parts.part2Up
                |> List.map printGleamExpressionNotParenthesized

        lineSpread : Print.LineSpread
        lineSpread =
            part0Print
                |> Print.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() -> part1Print |> Print.lineSpread)
                |> Print.lineSpreadMergeWith
                    (\() ->
                        part2UpPrints
                            |> Print.lineSpreadListMapAndCombine
                                Print.lineSpread
                    )
    in
    Print.exactly "#("
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                ((part0Print :: part1Print :: part2UpPrints)
                    |> Print.listIntersperseAndFlatten
                        (Print.exactly ","
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented lineSpread)
                        )
                )
            )
        |> Print.followedBy
            (Print.emptyOrLinebreakIndented lineSpread)
        |> Print.followedBy (Print.exactly ")")


printGleamExpressionCall :
    { called : GleamExpression
    , argument0 : GleamExpression
    , argument1Up : List GleamExpression
    }
    -> Print
printGleamExpressionCall call =
    let
        calledPrint : Print
        calledPrint =
            printGleamExpressionParenthesizedIfWithLetDeclarations
                call.called

        argumentPrints : List Print
        argumentPrints =
            (call.argument0 :: call.argument1Up)
                |> List.map printGleamExpressionParenthesizedIfWithLetDeclarations

        fullLineSpread : Print.LineSpread
        fullLineSpread =
            argumentPrints
                |> Print.lineSpreadListMapAndCombine Print.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() -> calledPrint |> Print.lineSpread)
    in
    calledPrint
        |> Print.followedBy (Print.exactly "(")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.emptyOrLinebreakIndented fullLineSpread
                    |> Print.followedBy
                        (argumentPrints
                            |> Print.listIntersperseAndFlatten
                                (Print.exactly ","
                                    |> Print.followedBy
                                        (Print.spaceOrLinebreakIndented fullLineSpread)
                                )
                        )
                )
            )
        |> Print.followedBy (Print.emptyOrLinebreakIndented fullLineSpread)
        |> Print.followedBy (Print.exactly ")")


gleamNumberLiteralToString : Float -> String
gleamNumberLiteralToString float =
    let
        floatAsString : String
        floatAsString =
            float |> String.fromFloat
    in
    if floatAsString |> String.contains "." then
        floatAsString

    else
        floatAsString ++ ".0"


printGleamExpressionList : List GleamExpression -> Print
printGleamExpressionList listElements =
    case listElements of
        [] ->
            Print.exactly "[]"

        element0 :: element1Up ->
            let
                elementsPrint : Print
                elementsPrint =
                    (element0 :: element1Up)
                        |> Print.listMapAndIntersperseAndFlatten
                            (\element ->
                                printGleamExpressionNotParenthesized element
                            )
                            (Print.exactly ","
                                |> Print.followedBy Print.linebreakIndented
                            )
            in
            Print.exactly "[ "
                |> Print.followedBy
                    (Print.withIndentIncreasedBy 2
                        elementsPrint
                    )
                |> Print.followedBy
                    (Print.spaceOrLinebreakIndented
                        (elementsPrint |> Print.lineSpread)
                    )
                |> Print.followedBy
                    (Print.exactly "]")


patternIsSpaceSeparated : GleamPattern -> Bool
patternIsSpaceSeparated gleamPattern =
    case gleamPattern of
        GleamPatternIgnore ->
            False

        GleamPatternNumber _ ->
            False

        GleamPatternChar _ ->
            False

        GleamPatternString _ ->
            False

        GleamPatternVariable _ ->
            False

        GleamPatternAs _ ->
            True

        GleamPatternListCons _ ->
            True

        GleamPatternListExact _ ->
            False

        GleamPatternVariant _ ->
            False

        GleamPatternTuple _ ->
            False


printGleamPatternParenthesizedIfSpaceSeparated : GleamPattern -> Print
printGleamPatternParenthesizedIfSpaceSeparated gleamPattern =
    if gleamPattern |> patternIsSpaceSeparated then
        printParenthesized
            { opening = "("
            , closing = ")"
            , inner = gleamPattern |> printGleamPatternNotParenthesized
            }

    else
        gleamPattern |> printGleamPatternNotParenthesized


parameterGeneratedGleamName : Int -> GleamPattern -> Maybe String
parameterGeneratedGleamName parameterIndex gleamPattern =
    case gleamPattern |> gleamPatternIntroducedVariables of
        [] ->
            Nothing

        variable0 :: variable1Up ->
            Just
                ("generated"
                    ++ (parameterIndex |> String.fromInt)
                    ++ "_"
                    ++ (variable0 :: variable1Up |> String.join "_")
                )


gleamPatternIntroducedVariables : GleamPattern -> List String
gleamPatternIntroducedVariables gleamPattern =
    -- IGNORE TCO
    case gleamPattern of
        GleamPatternNumber _ ->
            []

        GleamPatternChar _ ->
            []

        GleamPatternString _ ->
            []

        GleamPatternIgnore ->
            []

        GleamPatternVariable name ->
            [ name ]

        GleamPatternAs gleamPatternAs ->
            gleamPatternIntroducedVariables
                gleamPatternAs.pattern

        GleamPatternListExact elements ->
            elements
                |> List.concatMap gleamPatternIntroducedVariables

        GleamPatternVariant gleamPatternVariant ->
            gleamPatternVariant.values
                |> List.concatMap gleamPatternIntroducedVariables

        GleamPatternTuple parts ->
            (parts.part0 |> gleamPatternIntroducedVariables)
                ++ (parts.part1 |> gleamPatternIntroducedVariables)
                ++ (parts.part2Up
                        |> List.concatMap gleamPatternIntroducedVariables
                   )

        GleamPatternListCons gleamPatternListCons ->
            (case gleamPatternListCons.tail of
                Nothing ->
                    []

                Just variable ->
                    [ variable ]
            )
                ++ (gleamPatternListCons.initialElement0
                        |> gleamPatternIntroducedVariables
                   )
                ++ (gleamPatternListCons.initialElement1Up
                        |> List.concatMap gleamPatternIntroducedVariables
                   )


printGleamExpressionLambda :
    { parameter0 : Maybe String
    , parameter1Up : List (Maybe String)
    , result : GleamExpression
    }
    -> Print
printGleamExpressionLambda syntaxLambda =
    Print.exactly "fn("
        |> Print.followedBy
            ((syntaxLambda.parameter0 :: syntaxLambda.parameter1Up)
                |> Print.listMapAndIntersperseAndFlatten
                    (\parameterNameOrIgnored ->
                        case parameterNameOrIgnored of
                            Nothing ->
                                Print.exactly "_"

                            Just parameterName ->
                                Print.exactly parameterName
                    )
                    (Print.exactly ", ")
            )
        |> Print.followedBy (Print.exactly ") {")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printGleamExpressionNotParenthesized
                            syntaxLambda.result
                        )
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy (Print.exactly "}")


printGleamExpressionCase :
    { matched : GleamExpression
    , case0 : { pattern : GleamPattern, result : GleamExpression }
    , case1Up : List { pattern : GleamPattern, result : GleamExpression }
    }
    -> Print
printGleamExpressionCase gleamExpressionCase =
    let
        matchedPrint : Print
        matchedPrint =
            printGleamExpressionNotParenthesized gleamExpressionCase.matched

        matchedPrintLineSpread : Print.LineSpread
        matchedPrintLineSpread =
            matchedPrint |> Print.lineSpread
    in
    Print.exactly "case "
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.emptyOrLinebreakIndented matchedPrintLineSpread
                    |> Print.followedBy matchedPrint
                )
            )
        |> Print.followedBy
            (Print.emptyOrLinebreakIndented matchedPrintLineSpread)
        |> Print.followedBy (Print.exactly " {")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        ((gleamExpressionCase.case0 :: gleamExpressionCase.case1Up)
                            |> Print.listMapAndIntersperseAndFlatten
                                printGleamExpressionSingleCase
                                (Print.linebreak
                                    |> Print.followedBy Print.linebreakIndented
                                )
                        )
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy (Print.exactly "}")


printGleamExpressionWithLetDeclarations :
    { declaration0 : GleamLetDeclaration
    , declaration1Up : List GleamLetDeclaration
    , result : GleamExpression
    }
    -> Print
printGleamExpressionWithLetDeclarations syntaxLetIn =
    let
        letDestructurings :
            List
                { pattern : GleamPattern
                , expression : GleamExpression
                }
        letDestructurings =
            (syntaxLetIn.declaration0 :: syntaxLetIn.declaration1Up)
                |> List.filterMap
                    (\declaration ->
                        case declaration of
                            GleamLetDestructuring letDestructuring ->
                                Just letDestructuring

                            GleamLetDeclarationValueOrFunction _ ->
                                Nothing
                    )

        letValueOrFunctions :
            List
                { name : String
                , result : GleamExpression
                , type_ : Maybe GleamType
                }
        letValueOrFunctions =
            (syntaxLetIn.declaration0 :: syntaxLetIn.declaration1Up)
                |> List.filterMap
                    (\declaration ->
                        case declaration of
                            GleamLetDeclarationValueOrFunction letValueOrFunction ->
                                Just letValueOrFunction

                            GleamLetDestructuring _ ->
                                Nothing
                    )
    in
    (letDestructurings
        |> Print.listMapAndIntersperseAndFlatten
            (\letDestructuring ->
                (letDestructuring |> printGleamLetDestructuring)
                    |> Print.followedBy Print.linebreakIndented
                    |> Print.followedBy Print.linebreakIndented
            )
            Print.empty
    )
        |> Print.followedBy
            (letValueOrFunctions
                |> Print.listMapAndIntersperseAndFlatten
                    (\letValueOrFunction ->
                        (letValueOrFunction |> printGleamLetValueOrFunctionDeclaration)
                            |> Print.followedBy Print.linebreakIndented
                            |> Print.followedBy Print.linebreakIndented
                    )
                    Print.empty
            )
        |> Print.followedBy
            (printGleamExpressionNotParenthesized syntaxLetIn.result)


printGleamLetDestructuring :
    { pattern : GleamPattern, expression : GleamExpression }
    -> Print
printGleamLetDestructuring letDestructuring =
    Print.exactly "let "
        |> Print.followedBy
            (printGleamPatternNotParenthesized
                letDestructuring.pattern
            )
        |> Print.followedBy (Print.exactly " =")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printGleamExpressionNotParenthesized letDestructuring.expression)
                )
            )


printGleamExpressionSingleCase :
    { pattern : GleamPattern, result : GleamExpression }
    -> Print
printGleamExpressionSingleCase branch =
    let
        patternPrint : Print
        patternPrint =
            printGleamPatternNotParenthesized branch.pattern
    in
    Print.withIndentIncreasedBy 2
        patternPrint
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented
                (patternPrint |> Print.lineSpread)
            )
        |> Print.followedBy (Print.exactly "->")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printGleamExpressionParenthesizedIfWithLetDeclarations
                            branch.result
                        )
                )
            )


{-| Print value/function declarations into
an F# module called `Elm` in the global namespace that exposes all members.
Will also add some internal wrapper declarations.
-}
gleamDeclarationsToModuleString :
    { valuesAndFunctions :
        FastDict.Dict
            String
            { parameters : List (Maybe String)
            , result : GleamExpression
            , type_ : Maybe GleamType
            }
    , typeAliases :
        FastDict.Dict
            String
            { parameters : List String
            , type_ : GleamType
            }
    , recordTypes : FastSet.Set (List String)
    , choiceTypes :
        FastDict.Dict
            String
            { parameters : List String
            , variants : FastDict.Dict String (List GleamType)
            }
    }
    -> String
gleamDeclarationsToModuleString gleamDeclarations =
    let
        valueAndFunctionDeclarationsOrdered :
            List
                { name : String
                , parameters : List (Maybe String)
                , result : GleamExpression
                , type_ : Maybe GleamType
                }
        valueAndFunctionDeclarationsOrdered =
            gleamDeclarations.valuesAndFunctions
                |> fastDictMapAndToList
                    (\name valueOrFunctionInfo ->
                        { name = name
                        , type_ = valueOrFunctionInfo.type_
                        , parameters = valueOrFunctionInfo.parameters
                        , result = valueOrFunctionInfo.result
                        }
                    )

        typeAliasDeclarations :
            List
                { name : String
                , parameters : List String
                , type_ : GleamType
                }
        typeAliasDeclarations =
            gleamDeclarations.typeAliases
                |> fastDictMapAndToList
                    (\name info ->
                        { name = name
                        , parameters = info.parameters
                        , type_ = info.type_
                        }
                    )

        choiceTypeDeclarations :
            List
                { name : String
                , parameters : List String
                , variants : FastDict.Dict String (List GleamType)
                }
        choiceTypeDeclarations =
            gleamDeclarations.choiceTypes
                |> fastDictMapAndToList
                    (\name info ->
                        { name = name
                        , parameters = info.parameters
                        , variants = info.variants
                        }
                    )
    in
    defaultDeclarations
        ++ """

"""
        ++ (gleamDeclarations.recordTypes
                |> FastSet.foldr
                    (\recordTypeFields soFar ->
                        (Print.exactly "pub "
                            |> Print.followedBy
                                (printGleamRecordTypeDeclaration recordTypeFields)
                        )
                            :: soFar
                    )
                    []
                |> Print.listIntersperseAndFlatten
                    (Print.linebreak
                        |> Print.followedBy Print.linebreak
                    )
                |> Print.toString
           )
        ++ """

"""
        ++ (typeAliasDeclarations
                |> Print.listMapAndIntersperseAndFlatten
                    (\gleamTypeAliasDeclaration ->
                        Print.exactly "pub "
                            |> Print.followedBy
                                (printGleamTypeAliasDeclaration gleamTypeAliasDeclaration)
                    )
                    (Print.linebreak
                        |> Print.followedBy Print.linebreak
                    )
                |> Print.toString
           )
        ++ """

"""
        ++ (choiceTypeDeclarations
                |> Print.listMapAndIntersperseAndFlatten
                    (\gleamChoiceTypeDeclaration ->
                        Print.exactly "pub "
                            |> Print.followedBy
                                (printGleamChoiceTypeDeclaration gleamChoiceTypeDeclaration)
                    )
                    (Print.linebreak
                        |> Print.followedBy Print.linebreak
                    )
                |> Print.toString
           )
        ++ """


"""
        ++ ((valueAndFunctionDeclarationsOrdered
                |> Print.listMapAndIntersperseAndFlatten
                    (\gleamValueOrFunctionDeclaration ->
                        Print.exactly "pub fn "
                            |> Print.followedBy
                                (Print.exactly (gleamValueOrFunctionDeclaration.name ++ "()")
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            ((case gleamValueOrFunctionDeclaration.type_ of
                                                Nothing ->
                                                    Print.empty

                                                Just declaredType ->
                                                    let
                                                        typePrint : Print
                                                        typePrint =
                                                            printGleamTypeNotParenthesized declaredType

                                                        typeLineSpread : Print.LineSpread
                                                        typeLineSpread =
                                                            typePrint |> Print.lineSpread
                                                    in
                                                    Print.exactly " ->"
                                                        |> Print.followedBy
                                                            (Print.spaceOrLinebreakIndented typeLineSpread
                                                                |> Print.followedBy
                                                                    (Print.withIndentAtNextMultipleOf4
                                                                        typePrint
                                                                    )
                                                            )
                                             )
                                                |> Print.followedBy
                                                    (Print.exactly " {")
                                                |> Print.followedBy Print.linebreakIndented
                                                |> Print.followedBy
                                                    (printGleamExpressionNotParenthesized
                                                        (case gleamValueOrFunctionDeclaration.parameters of
                                                            [] ->
                                                                gleamValueOrFunctionDeclaration.result

                                                            parameter0 :: parameter1Up ->
                                                                GleamExpressionLambda
                                                                    { parameter0 = parameter0
                                                                    , parameter1Up = parameter1Up
                                                                    , result = gleamValueOrFunctionDeclaration.result
                                                                    }
                                                        )
                                                    )
                                                |> Print.followedBy Print.linebreak
                                                |> Print.followedBy (Print.exactly "}")
                                            )
                                        )
                                )
                    )
                    (Print.linebreak
                        |> Print.followedBy Print.linebreak
                    )
            )
                |> Print.toString
           )
        ++ """
"""


defaultDeclarations : String
defaultDeclarations =
    """import gleam/bool
import gleam/dict
import gleam/float
import gleam/function
import gleam/int
import gleam/list
import gleam/option
import gleam/order
import gleam/string

fn basics_eq(a: a, b: a) -> Bool {
  a == b
}

fn basics_neq(a: a, b: a) -> Bool {
  a != b
}

fn basics_lt(a: Float, b: Float) -> Bool {
  a <. b
}

fn basics_gt(a: Float, b: Float) -> Bool {
  a >. b
}

fn basics_le(a: Float, b: Float) -> Bool {
  a <=. b
}

fn basics_ge(a: Float, b: Float) -> Bool {
  a >=. b
}

fn basics_truncate(n: Float) -> Float {
  int.to_float(float.truncate(n))
}

fn basics_round(n: Float) -> Float {
  int.to_float(float.round(n))
}

fn basics_mod_by(divisor: Float, to_divide: Float) -> Float {
  case float.modulo(to_divide, by: divisor) {
    Error(_) -> 0.0
    Ok(remainder) -> remainder
  }
}

fn basics_remainder_by(divisor: Float, to_divide: Float) -> Float {
  let remainder = basics_mod_by(to_divide, divisor)

  case
    { remainder >. 0.0 && divisor <. 0.0 }
    || { remainder <. 0.0 && divisor >. 0.0 }
  {
    True -> remainder -. to_divide
    False -> remainder
  }
}

fn basics_idiv() -> fn(Float, Float) -> Float {
  fn(to_divide, divisor) {
    case float.divide(to_divide, by: divisor) {
      Error(_) -> 0.0
      Ok(division_result) -> int.to_float(float.truncate(division_result))
    }
  }
}

fn basics_fdiv(to_divide: Float, divisor: Float) -> Float {
  case float.divide(to_divide, by: divisor) {
    Error(_) -> 0.0
    Ok(division_result) -> division_result
  }
}

fn basics_always_false(a: a) -> Bool {
  False
}

fn string_contains(needle: String, str: String) -> Bool {
  string.contains(str, needle)
}

fn string_drop_left(count_to_skip: Float, str: String) -> String {
  string.drop_start(str, float.truncate(count_to_skip))
}

fn string_drop_right(count_to_skip: Float, str: String) -> String {
  string.drop_end(str, float.truncate(count_to_skip))
}

fn string_left(count_to_take: Float, str: String) -> String {
  string.drop_end(str, string.length(str) - float.truncate(count_to_take))
}

fn string_right(count_to_take: Float, str: String) -> String {
  string.drop_start(str, string.length(str) - float.truncate(count_to_take))
}

fn string_pad_right(
  desired_length: Float,
  pad_string: String,
  str: String,
) -> String {
  string.pad_end(str, float.truncate(desired_length), pad_string)
}

fn string_repeat(repetition_count: Float, segment: String) -> String {
  string.repeat(segment, float.truncate(repetition_count))
}

fn string_replace(
  to_replace: String,
  replacement: String,
  str: String,
) -> String {
  string.replace(str, to_replace, replacement)
}

fn string_slice(start: Float, end: Float, str: String) -> String {
  string.slice(str, float.truncate(start), float.truncate(end))
}

fn string_split(separator: String, str: String) -> List(String) {
  string.split(str, on: separator)
}

fn string_starts_with(start: String, str: String) -> Bool {
  string.starts_with(str, start)
}

fn string_ends_with(end: String, str: String) -> Bool {
  string.ends_with(str, end)
}

fn char_to_lower(char: Int) -> Int {
  case string.utf_codepoint(char) {
    Error(Nil) -> char
    Ok(code_point) ->
      case
        string.to_utf_codepoints(
          string.lowercase(string.from_utf_codepoints([code_point])),
        )
      {
        [] -> panic
        [first_lower_code_point, ..] ->
          string.utf_codepoint_to_int(first_lower_code_point)
      }
  }
}

fn char_to_upper(char: Int) -> Int {
  case string.utf_codepoint(char) {
    Error(Nil) -> char
    Ok(code_point) ->
      case
        string.to_utf_codepoints(
          string.lowercase(string.from_utf_codepoints([code_point])),
        )
      {
        [] -> panic
        [first_lower_code_point, ..] ->
          string.utf_codepoint_to_int(first_lower_code_point)
      }
  }
}

fn string_from_char(char: Int) -> String {
  case string.utf_codepoint(char) {
    Error(Nil) -> "\\u{0}"
    Ok(code_point) -> string.from_utf_codepoints([code_point])
  }
}

fn string_all(is_expected: fn(Int) -> Bool, string: String) -> Bool {
  list.all(string.to_utf_codepoints(string), fn(code_point) {
    is_expected(string.utf_codepoint_to_int(code_point))
  })
}

fn string_any(is_needle: fn(Int) -> Bool, string: String) -> Bool {
  list.any(string.to_utf_codepoints(string), fn(code_point) {
    is_needle(string.utf_codepoint_to_int(code_point))
  })
}

fn string_join(separator: String, strings: List(String)) -> String {
  string.join(strings, with: separator)
}

fn string_filter(should_keep: fn(Int) -> Bool, str: String) -> String {
  string.from_utf_codepoints(
    list.filter(string.to_utf_codepoints(str), fn(code_point) {
      should_keep(string.utf_codepoint_to_int(code_point))
    }),
  )
}

fn string_to_int(str: String) -> option.Option(Float) {
  case int.parse(str) {
    Ok(parsed_int) -> option.Some(int.to_float(parsed_int))
    Error(Nil) -> option.None
  }
}

fn string_to_float(str: String) -> option.Option(Float) {
  case float.parse(str) {
    Ok(parsed_float) -> option.Some(parsed_float)
    Error(Nil) -> option.None
  }
}

fn string_cons(new_head_char: Int, tail: String) -> String {
  string.append(string_from_char(new_head_char), tail)
}

fn list_cons(new_head: a, tail: List(a)) -> List(a) {
  list.prepend(tail, new_head)
}

fn list_all(is_expected: fn(a) -> Bool, list: List(a)) -> Bool {
  list.all(list, is_expected)
}

fn list_any(is_needle: fn(a) -> Bool, list: List(a)) -> Bool {
  list.any(list, is_needle)
}

fn list_member(needle: a, list: List(a)) -> Bool {
  list.contains(list, any: needle)
}

fn list_drop(count_to_skip: Float, list: List(a)) -> List(a) {
  list.drop(list, float.truncate(count_to_skip))
}

fn list_take(count_to_keep: Float, list: List(a)) -> List(a) {
  list.take(list, float.truncate(count_to_keep))
}

fn list_intersperse(in_between: a, list: List(a)) -> List(a) {
  list.intersperse(list, in_between)
}

fn list_partition(
  categorize: fn(a) -> Bool,
  list: List(a),
) -> #(List(a), List(a)) {
  list.partition(list, categorize)
}

fn list_map(element_change: fn(a) -> b, list: List(a)) -> List(b) {
  list.map(list, element_change)
}

fn list_map2(
  combine_ab: fn(a, b) -> c,
  a_list: List(a),
  b_list: List(b),
) -> List(c) {
  list.map2(a_list, b_list, combine_ab)
}

fn list_filter_map(
  element_to_list: fn(a) -> option.Option(b),
  list: List(a),
) -> List(b) {
  list.filter_map(list, fn(element) {
    option.to_result(element_to_list(element), Nil)
  })
}

fn list_concat_map(element_to_list: fn(a) -> List(b), list: List(a)) -> List(b) {
  list.flat_map(list, element_to_list)
}

fn list_filter(keep_element: fn(a) -> Bool, list: List(a)) -> List(a) {
  list.filter(list, keep_element)
}

fn list_repeat(final_length: Float, element_to_repeat: a) -> List(a) {
  list.repeat(element_to_repeat, float.truncate(final_length))
}

fn list_foldl(
  reduce: fn(a, folded) -> folded,
  initial_folded: folded,
  list: List(a),
) -> folded {
  list.fold(list, initial_folded, fn(so_far, element) {
    reduce(element, so_far)
  })
}

fn list_foldr(
  reduce: fn(a, folded) -> folded,
  initial_folded: folded,
  list: List(a),
) -> folded {
  list.fold_right(list, initial_folded, fn(so_far, element) {
    reduce(element, so_far)
  })
}

fn list_range(start: Float, end: Float) -> List(Float) {
  let start_int = float.truncate(start)
  let end_int = float.truncate(end)

  case start_int > end_int {
    True -> []
    False -> list.map(list.range(start_int, end_int), int.to_float)
  }
}

fn list_sort_with(
  element_compare: fn(a, a) -> order.Order,
  list: List(a),
) -> List(a) {
  list.sort(list, element_compare)
}

fn list_sort(list: List(Float)) -> List(Float) {
  list.sort(list, float.compare)
}

fn list_maximum(list: List(Float)) -> option.Option(Float) {
  option.from_result(list.max(list, float.compare))
}

fn list_minimum(list: List(Float)) -> option.Option(Float) {
  option.from_result(list.max(list, order.reverse(float.compare)))
}
"""


resultAndThen3 :
    (a -> b -> c -> Result error d)
    -> Result error a
    -> Result error b
    -> Result error c
    -> Result error d
resultAndThen3 abToResult aResult bResult cResult =
    case aResult of
        Err error ->
            Err error

        Ok a ->
            case bResult of
                Err error ->
                    Err error

                Ok b ->
                    case cResult of
                        Err error ->
                            Err error

                        Ok c ->
                            abToResult a b c


fastDictMapAndToList :
    (key -> value -> element)
    -> FastDict.Dict key value
    -> List element
fastDictMapAndToList keyValueToElement fastDict =
    fastDict
        |> FastDict.foldr
            (\key value soFar ->
                keyValueToElement key value
                    :: soFar
            )
            []


listMapAndCombineOk : (a -> Result err ok) -> List a -> Result err (List ok)
listMapAndCombineOk elementToResult list =
    listMapAndCombineOkFrom [] elementToResult list


listMapAndCombineOkFrom : List ok -> (a -> Result err ok) -> List a -> Result err (List ok)
listMapAndCombineOkFrom soFar elementToResult list =
    case list of
        [] ->
            Ok (soFar |> List.reverse)

        head :: tail ->
            case head |> elementToResult of
                Err headErr ->
                    Err headErr

                Ok headOk ->
                    listMapAndCombineOkFrom (headOk :: soFar)
                        elementToResult
                        tail
