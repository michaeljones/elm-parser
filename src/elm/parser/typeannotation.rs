use combine::error::StreamError;
use combine::{ParseError, Parser, RangeStream};

use super::base;
use super::tokens;
use super::whitespace;
use elm::syntax::typeannotation::TypeAnnotation;

pub fn type_annotation_<'a, I>() -> impl Parser<Input = I, Output = TypeAnnotation> + 'a
where
    I: 'a,
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    combine::choice((
        combine::attempt(function_type_annotation()),
        unit(),
        parens_type_annotation(),
        tupled_type_annotation(),
        typed_type_annotation(),
        generic_type_annotation(),
        // recordTypeAnnotation
    ))
}

// Wrapper for type_annotation to allow it to be called recursively
parser!{
    pub fn type_annotation['a, I]()(I) -> TypeAnnotation
    where [I: 'a, I: RangeStream<Item = char, Range = &'a str>]
    {
        type_annotation_()
    }
}

pub fn non_function_type_annotation_<'a, I>() -> impl Parser<Input = I, Output = TypeAnnotation> + 'a
where
    I: 'a,
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    combine::choice((
        unit(),
        parens_type_annotation(),
        tupled_type_annotation(),
        typed_type_annotation(),
        generic_type_annotation(),
        // recordTypeAnnotation
    ))
}

// Wrapper for type_annotation to allow it to be called recursively
parser!{
    pub fn non_function_type_annotation['a, I]()(I) -> TypeAnnotation
    where [I: 'a, I: RangeStream<Item = char, Range = &'a str>]
    {
        non_function_type_annotation_()
    }
}

pub fn parens_type_annotation<'a, I>() -> impl Parser<Input = I, Output = TypeAnnotation> + 'a
where
    I: 'a,
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    combine::between(combine::token('('), combine::token(')'), type_annotation())
}

pub fn unit<'a, I>() -> impl Parser<Input = I, Output = TypeAnnotation> + 'a
where
    I: 'a,
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    combine::parser::char::string("()").map(|_| TypeAnnotation::Unit)
}

pub fn tupled_type_annotation<'a, I>() -> impl Parser<Input = I, Output = TypeAnnotation> + 'a
where
    I: 'a,
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let comma_then_type = combine::token(',').with(type_annotation());
    let contents = type_annotation().and(combine::many1(comma_then_type));

    combine::between(combine::token('('), combine::token(')'), contents).map(
        |(first, mut rest): (TypeAnnotation, Vec<TypeAnnotation>)| {
            rest.insert(0, first);
            TypeAnnotation::Tupled(rest)
        },
    )
}

pub fn typed_type_annotation<'a, I>() -> impl Parser<Input = I, Output = TypeAnnotation> + 'a
where
    I: 'a,
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    base::module_name()
        .and(combine::optional(whitespace::many1_spaces().with(
            combine::sep_by(non_function_type_annotation(), whitespace::many1_spaces()),
        ))).and_then(
            |(mut module_name, args): (Vec<String>, Option<Vec<TypeAnnotation>>)| match module_name
                .pop()
            {
                Some(name) => Ok(TypeAnnotation::Typed(
                    (module_name, name),
                    args.unwrap_or(vec![]),
                )),
                None => Err(
                    combine::stream::StreamErrorFor::<I>::expected_static_message(
                        "Nothing in module name",
                    ),
                ),
            },
        )
}

pub fn generic_type_annotation<'a, I>() -> impl Parser<Input = I, Output = TypeAnnotation> + 'a
where
    I: 'a,
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    tokens::function_name().map(TypeAnnotation::GenericType)
}

pub fn function_type_annotation<'a, I>() -> impl Parser<Input = I, Output = TypeAnnotation> + 'a
where
    I: 'a,
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    non_function_type_annotation()
        .skip(combine::parser::char::string(" -> "))
        .and(type_annotation())
        .map(|(first, second): (TypeAnnotation, TypeAnnotation)| {
            TypeAnnotation::FunctionTypeAnnotation(Box::new(first), Box::new(second))
        })
}

#[cfg(test)]
mod tests {

    use super::*;
    use combine::Parser;

    #[test]
    fn generic_type_annotation_1() {
        assert_eq!(
            generic_type_annotation().parse("abc"),
            Ok((TypeAnnotation::GenericType("abc".to_string()), ""))
        );
    }

    #[test]
    fn unit_1() {
        assert_eq!(unit().parse("()"), Ok((TypeAnnotation::Unit, "")));
    }

    #[test]
    fn tupled_type_annotation_1() {
        assert_eq!(
            tupled_type_annotation().parse("(a,b)"),
            Ok((
                TypeAnnotation::Tupled(vec![
                    TypeAnnotation::GenericType("a".to_string()),
                    TypeAnnotation::GenericType("b".to_string())
                ]),
                ""
            ))
        );
    }

    #[test]
    fn typed_type_annotation_1() {
        assert_eq!(
            typed_type_annotation().parse("Aaa"),
            Ok((
                TypeAnnotation::Typed((vec![], "Aaa".to_string()), vec![]),
                ""
            ))
        );
    }

    #[test]
    fn typed_type_annotation_1_non_function() {
        assert_eq!(
            non_function_type_annotation().parse("Aaa"),
            Ok((
                TypeAnnotation::Typed((vec![], "Aaa".to_string()), vec![]),
                ""
            ))
        );
    }

    #[test]
    fn typed_type_annotation_1_full() {
        assert_eq!(
            type_annotation().parse("Aaa"),
            Ok((
                TypeAnnotation::Typed((vec![], "Aaa".to_string()), vec![]),
                ""
            ))
        );
    }

    #[test]
    fn typed_type_annotation_2() {
        assert_eq!(
            typed_type_annotation().parse("Aaa.Bbb Ccc"),
            Ok((
                TypeAnnotation::Typed(
                    (vec!["Aaa".to_string()], "Bbb".to_string()),
                    vec![TypeAnnotation::Typed((vec![], "Ccc".to_string()), vec![])]
                ),
                ""
            ))
        );
    }

    #[test]
    fn function_type_annotation_1() {
        assert_eq!(
            function_type_annotation().parse("() -> ()"),
            Ok((
                TypeAnnotation::FunctionTypeAnnotation(
                    Box::new(TypeAnnotation::Unit),
                    Box::new(TypeAnnotation::Unit)
                ),
                ""
            ))
        );
    }

    #[test]
    fn function_type_annotation_2() {
        assert_eq!(
            function_type_annotation().parse("a -> b"),
            Ok((
                TypeAnnotation::FunctionTypeAnnotation(
                    Box::new(TypeAnnotation::GenericType("a".to_string())),
                    Box::new(TypeAnnotation::GenericType("b".to_string()))
                ),
                ""
            ))
        );
    }

    #[test]
    fn function_type_annotation_3() {
        assert_eq!(
            function_type_annotation().parse("a -> b -> c"),
            Ok((
                TypeAnnotation::FunctionTypeAnnotation(
                    Box::new(TypeAnnotation::GenericType("a".to_string())),
                    Box::new(TypeAnnotation::FunctionTypeAnnotation(
                        Box::new(TypeAnnotation::GenericType("b".to_string())),
                        Box::new(TypeAnnotation::GenericType("c".to_string()))
                    )),
                ),
                ""
            ))
        );
    }
}
/*
module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNonGreedy)

import Combine exposing (..)
import Elm.Parser.Base exposing (typeIndicator)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Ranges
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range
import Elm.Syntax.TypeAnnotation exposing (..)


type Mode
    = Eager
    | Lazy


typeAnnotation : Parser State (Node TypeAnnotation)
typeAnnotation =
    lazy
        (\() ->
            typeAnnotationNoFn Eager
                |> Combine.andThen
                    (\typeRef ->
                        Layout.optimisticLayoutWith
                            (\() -> succeed typeRef)
                            (\() ->
                                or
                                    (Combine.map (\ta -> Node.combine FunctionTypeAnnotation typeRef ta)
                                        (string "->"
                                            |> Combine.ignore (maybe Layout.layout)
                                            |> Combine.continueWith typeAnnotation
                                        )
                                    )
                                    (succeed typeRef)
                            )
                    )
        )


typeAnnotationNonGreedy : Parser State (Node TypeAnnotation)
typeAnnotationNonGreedy =
    choice
        [ parensTypeAnnotation
        , typedTypeAnnotation Lazy
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]


typeAnnotationNoFn : Mode -> Parser State (Node TypeAnnotation)
typeAnnotationNoFn mode =
    lazy
        (\() ->
            choice
                [ parensTypeAnnotation
                , typedTypeAnnotation mode
                , genericTypeAnnotation
                , recordTypeAnnotation
                ]
        )


parensTypeAnnotation : Parser State (Node TypeAnnotation)
parensTypeAnnotation =
    lazy
        (\() ->
            let
                commaSep : Parser State (List (Node TypeAnnotation))
                commaSep =
                    many
                        (string ","
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.continueWith typeAnnotation
                            |> Combine.ignore (maybe Layout.layout)
                        )

                nested : Parser State TypeAnnotation
                nested =
                    Combine.succeed asTypeAnnotation
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andMap typeAnnotation
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andMap commaSep
            in
            Node.parser
                (Combine.string "("
                    |> Combine.continueWith
                        (Combine.choice
                            [ Combine.string ")" |> Combine.map (always Unit)
                            , nested |> Combine.ignore (Combine.string ")")
                            ]
                        )
                )
        )


asTypeAnnotation : Node TypeAnnotation -> List (Node TypeAnnotation) -> TypeAnnotation
asTypeAnnotation ((Node range value) as x) xs =
    case xs of
        [] ->
            value

        _ ->
            Tupled (x :: xs)


genericTypeAnnotation : Parser State (Node TypeAnnotation)
genericTypeAnnotation =
    lazy
        (\() ->
            Node.parser (Combine.map GenericType functionName)
        )


recordFieldsTypeAnnotation : Parser State RecordDefinition
recordFieldsTypeAnnotation =
    lazy (\() -> sepBy (string ",") (Layout.maybeAroundBothSides <| Node.parser recordFieldDefinition))


recordTypeAnnotation : Parser State (Node TypeAnnotation)
recordTypeAnnotation =
    lazy
        (\() ->
            let
                nextField : Parser State RecordField
                nextField =
                    Combine.succeed (\a b -> ( a, b ))
                        |> Combine.ignore (Combine.string ",")
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andMap (Node.parser functionName)
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.ignore (string ":")
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andMap typeAnnotation
                        |> Combine.ignore Layout.optimisticLayout

                additionalRecordFields : RecordDefinition -> Parser State RecordDefinition
                additionalRecordFields items =
                    Combine.choice
                        [ Node.parser nextField
                            |> Combine.andThen (\next -> additionalRecordFields (next :: items))
                        , Combine.succeed (List.reverse items)
                        ]
            in
            Node.parser
                (string "{"
                    |> Combine.ignore (maybe Layout.layout)
                    |> Combine.continueWith
                        (Combine.choice
                            [ Combine.string "}" |> Combine.continueWith (Combine.succeed (Record []))
                            , Node.parser functionName
                                |> Combine.ignore (maybe Layout.layout)
                                |> Combine.andThen
                                    (\fname ->
                                        Combine.choice
                                            [ Combine.succeed (GenericRecord fname)
                                                |> Combine.ignore (Combine.string "|")
                                                |> Combine.andMap (Node.parser recordFieldsTypeAnnotation)
                                                |> Combine.ignore (Combine.string "}")
                                            , Combine.string ":"
                                                |> Combine.ignore (maybe Layout.layout)
                                                |> Combine.continueWith typeAnnotation
                                                |> Combine.ignore (maybe Layout.layout)
                                                |> Combine.andThen
                                                    (\ta ->
                                                        additionalRecordFields [ Node.combine Tuple.pair fname ta ]
                                                            |> Combine.map Record
                                                    )
                                                |> Combine.ignore (Combine.string "}")
                                            ]
                                    )
                            ]
                        )
                )
        )


recordFieldDefinition : Parser State RecordField
recordFieldDefinition =
    lazy
        (\() ->
            succeed Tuple.pair
                |> Combine.andMap (maybe Layout.layout |> Combine.continueWith (Node.parser functionName))
                |> Combine.andMap
                    (maybe Layout.layout
                        |> Combine.continueWith (string ":")
                        |> Combine.continueWith (maybe Layout.layout)
                        |> Combine.continueWith typeAnnotation
                    )
        )


typedTypeAnnotation : Mode -> Parser State (Node TypeAnnotation)
typedTypeAnnotation mode =
    lazy
        (\() ->
            let
                genericHelper : List (Node TypeAnnotation) -> Parser State (List (Node TypeAnnotation))
                genericHelper items =
                    or
                        (typeAnnotationNoFn Lazy
                            |> Combine.andThen
                                (\next ->
                                    Layout.optimisticLayoutWith
                                        (\() -> Combine.succeed (List.reverse (next :: items)))
                                        (\() -> genericHelper (next :: items))
                                        |> Combine.ignore (maybe Layout.layout)
                                )
                        )
                        (Combine.succeed (List.reverse items))

                nodeRanges =
                    List.map (\(Node r _) -> r)
            in
            Node.parser typeIndicator
                |> Combine.andThen
                    (\((Node tir ( m, fn )) as original) ->
                        Layout.optimisticLayoutWith
                            (\() -> Combine.succeed (Node tir (Typed original [])))
                            (\() ->
                                case mode of
                                    Eager ->
                                        Combine.map
                                            (\args ->
                                                Node
                                                    (Range.combine (tir :: nodeRanges args))
                                                    (Typed original args)
                                            )
                                            (genericHelper [])

                                    Lazy ->
                                        Combine.succeed (Node tir (Typed original []))
                            )
                    )
        )
        */
