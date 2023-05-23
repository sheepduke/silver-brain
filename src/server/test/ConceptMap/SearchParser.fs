namespace SilverBrain.Test.ConceptMapTests

open NUnit.Framework

open SilverBrain.Domain.ConceptMap.SearchParser

module SearchParserTests =
    let (=>) input expected =
        let actual = parse input

        match (actual = Ok(expected)) with
        | true -> ()
        | false -> failwith (sprintf "Input: %A\nExpected: %A\nActual: %A" input (Ok(expected)) actual)

    let sl (query: string) : Query = StringQuery(LiteralString query)

    let sq (query: string) : Query = StringQuery(QuotedString query)

    let ls (query: string) : AnyString = LiteralString query

    let qs (query: string) : AnyString = QuotedString query

    let p (key: AnyString) (op: ComparisonOperator) (value: AnyString) : Query = PropertyFilter(op, key, value)

    let t (key: AnyString) (op: ComparisonOperator) (value: AnyString) : Query = TagFilter(op, key, value)

    let c (key: AnyString) (value: AnyString) : Query = Condition(key, value)

    let a (left: Query) (right: Query) : Query = AndQuery(left, right)

    let o (left: Query) (right: Query) : Query = OrQuery(left, right)

    let n (query: Query) = NotQuery query

    let (eq, ne, lt, le, gt, ge) =
        (Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual)

    [<Test>]
    let ``parser`` () =
        // Basic.
        " one" => sl "one"
        "one " => sl "one"
        " one " => sl "one"
        "one" => sl "one"

        // Quoted string.
        "\"quo\\\"ted*str\\\\ing\"" => sq """quo"ted*str\ing"""
        "\"one AND two\"" => sq "one AND two"
        "\"one && two\"" => sq "one && two"

        // Condition.
        "is: relation" => c (ls "is") (ls "relation")
        "is:relation" => c (ls "is") (ls "relation")
        "is :relation" => c (ls "is") (ls "relation")
        "is : relation" => c (ls "is") (ls "relation")

        // Property.
        "prop :<= value" => p (ls "prop") le (ls "value")
        "prop:<= value" => p (ls "prop") le (ls "value")
        "prop :<=value" => p (ls "prop") le (ls "value")
        "prop:<=value" => p (ls "prop") le (ls "value")
        "prop :<= \"value\"" => p (ls "prop") le (qs "value")
        "prop :< value" => p (ls "prop") lt (ls "value")
        "prop := value" => p (ls "prop") eq (ls "value")
        "prop :<> value" => p (ls "prop") ne (ls "value")
        "prop :>= value" => p (ls "prop") ge (ls "value")
        "prop :> value" => p (ls "prop") gt (ls "value")

        // Tag.
        "tag = value" => t (ls "tag") eq (ls "value")
        "tag= value" => t (ls "tag") eq (ls "value")
        "tag =value" => t (ls "tag") eq (ls "value")
        "tag=value" => t (ls "tag") eq (ls "value")
        "tag <> value" => t (ls "tag") ne (ls "value")
        "tag < value" => t (ls "tag") lt (ls "value")
        "tag <= value" => t (ls "tag") le (ls "value")
        "tag > value" => t (ls "tag") gt (ls "value")
        "tag >= value" => t (ls "tag") ge (ls "value")
        "one two" => a (sl "one") (sl "two")
        "one \"two\" three" => a (sl "one") (a (sq "two") (sl "three"))
        "one \"two three\"" => a (sl "one") (sq "two three")
        "one&&two" => a (sl "one") (sl "two")
        "one &&two" => a (sl "one") (sl "two")
        "one&& two" => a (sl "one") (sl "two")
        "one && two" => a (sl "one") (sl "two")
        "one AND two" => a (sl "one") (sl "two")
        "oneANDtwo" => sl "oneANDtwo"
        "one ANDtwo" => a (sl "one") (sl "ANDtwo")
        "oneAND two" => a (sl "oneAND") (sl "two")
        "one AND two" => a (sl "one") (sl "two")
        "one  AND  two" => a (sl "one") (sl "two")

        // OR.
        "one || two" => o (sl "one") (sl "two")
        "one|| two" => o (sl "one") (sl "two")
        "one ||two" => o (sl "one") (sl "two")
        "one||two" => o (sl "one") (sl "two")
        "one || two" => o (sl "one") (sl "two")
        "one OR two" => o (sl "one") (sl "two")
        "oneOR two" => a (sl "oneOR") (sl "two")
        "one OR \"two\"" => o (sl "one") (sq "two")

        // NOT.
        "NOT one" => n (sl "one")
        "one NOT two" => a (sl "one") (n (sl "two"))
        "NOT one two" => a (n (sl "one")) (sl "two")
        "! one" => n (sl "one")
        "!one" => n (sl "one")
        "!  one two" => a (n (sl "one")) (sl "two")

        // Composed.
        "Emacs is : editor AND free = true OR name := vim &&\"true\" NOT false"
        => o
            (a (sl "Emacs") (a (c (ls "is") (ls "editor")) (t (ls "free") eq (ls "true"))))
            (a (p (ls "name") eq (ls "vim")) (a (sq "true") (n (sl "false"))))
