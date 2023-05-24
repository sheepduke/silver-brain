namespace SilverBrain.Domain.ConceptMap

open FSharpPlus
open FParsec

module SearchParser =
    type ComparisonOperator =
        | Equal
        | NotEqual
        | Less
        | LessEqual
        | GreaterEqual
        | Greater

    type AnyString =
        | LiteralString of string
        | QuotedString of string

        member this.String: string =
            match this with
            | LiteralString value -> value
            | QuotedString value -> value

    type StringQuery = AnyString
    type PropertyFilter = ComparisonOperator * AnyString * AnyString
    type TagFilter = ComparisonOperator * AnyString * AnyString
    type Condition = AnyString * AnyString

    type Query =
        | StringQuery of StringQuery
        | PropertyFilter of PropertyFilter
        | TagFilter of TagFilter
        | Condition of Condition
        | AndQuery of (Query * Query)
        | OrQuery of (Query * Query)
        | NotQuery of Query

    type AndQuery = Query * Query
    type OrQuery = Query * Query
    type NotQuery = Query

    module private Internal =

        // ------------------------------------------------------------
        //  Basic
        // ------------------------------------------------------------

        let ws = spaces

        let ws1 = skipMany1 (anyOf " \r\n\t")

        let basicString = many1Chars (noneOf " :=<>()&|") |>> LiteralString

        let quote = pstring "\""

        let quotedString =
            let normalChar = satisfy (fun ch -> ch <> '\\' && ch <> '\"')

            let escapedChar = pstring "\\" >>. (anyOf "\\\"")

            between quote quote (manyChars (normalChar <|> escapedChar)) |>> QuotedString

        let anyString = quotedString <|> basicString

        // ------------------------------------------------------------
        //  Property Filter
        // ------------------------------------------------------------

        let propertyEqual = pstring ":=" >>% Equal

        let propertyNotEqual = pstring ":<>" >>% NotEqual

        let propertyLess = pstring ":<" >>% Less

        let propertyLessEqual = pstring ":<=" >>% LessEqual

        let propertyGreater = pstring ":>" >>% Greater

        let propertyGreaterEqual = pstring ":>=" >>% GreaterEqual

        let propertyFilterOperator =
            propertyNotEqual
            <|> propertyLessEqual
            <|> propertyLess
            <|> propertyGreaterEqual
            <|> propertyGreater
            <|> propertyEqual

        let propertyFilter =
            pipe3 anyString (ws >>. propertyFilterOperator .>> ws) anyString (fun left op right ->
                PropertyFilter(op, left, right))

        // ------------------------------------------------------------
        //  Condition
        // ------------------------------------------------------------

        let conditionOp = ws >>. pstring ":" .>> ws

        let condition =
            pipe3 anyString conditionOp anyString (fun left _ right -> Condition(left, right))

        // ------------------------------------------------------------
        //  Tag
        // ------------------------------------------------------------

        let tagEqual = pstring "=" >>% Equal

        let tagNotEqual = pstring "<>" >>% NotEqual

        let tagLess = pstring "<" >>% Less

        let tagLessEqual = pstring "<=" >>% LessEqual

        let tagGreater = pstring ">" >>% Greater

        let tagGreaterEqual = pstring ">=" >>% GreaterEqual

        let tagFilterOperator =
            tagEqual
            <|> tagNotEqual
            <|> tagLessEqual
            <|> tagLess
            <|> tagGreaterEqual
            <|> tagGreater

        let tagFilter =
            pipe3 anyString (ws >>. tagFilterOperator .>> ws) anyString (fun left op right ->
                TagFilter(op, left, right))

        // ------------------------------------------------------------
        //  Query
        // ------------------------------------------------------------

        let query, queryRef = createParserForwardedToRef<Query, unit> ()

        let singleQuery =
            attempt propertyFilter
            <|> attempt tagFilter
            <|> attempt condition
            <|> (anyString |>> StringQuery)

        let bracedQuery = pstring "(" >>. ws >>. query .>> ws .>> pstring ")"

        let primaryQuery = bracedQuery <|> singleQuery

        let notQuery =
            (pstring "!" <|> pstring "NOT ") >>. ws >>. primaryQuery |>> NotQuery
            <|> primaryQuery

        let orOp =
            ws >>. (skipString "||" <|> skipString "OR ") .>> ws
            >>% (fun left right -> OrQuery(left, right))

        let andOp =
            attempt (ws >>. (skipString "&&" <|> skipString "AND ") .>> ws)
            <|> attempt (ws1 >>. notFollowedBy orOp >>. followedBy anyString)
            >>% (fun left right -> AndQuery(left, right))

        let andQuery = chainr1 notQuery andOp

        let orQuery = chainr1 andQuery orOp

        let wholeQuery = query .>> eof

        do queryRef.Value <- orQuery

    let parse (query: string) : Result<Query, string> =
        match run Internal.wholeQuery (String.trim " " query) with
        | Success(query, _, _) -> Result.Ok query
        | Failure(error, _, _) -> Result.Error error
