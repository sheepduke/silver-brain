defmodule SilverBrain.Core.SearchQuery do
  @moduledoc """
  A query might be one of the following:
  - {:and, [query]}
  - {:or, [query]}
  - {:not, [query]}
  - {:match, "key", "value"}
  - {:less_than, "key", "value"}
  - {:less_equal, "key", "value"}
  - {:equal, "key", "value"}
  - {:greater_than, "key", "value"}
  - {:greater_equal, "key", "value"}
  - "keyword"
  """

  import NimbleParsec

  def parse(search_string) do
    case String.trim(search_string) do
      "" ->
        nil

      trimmed_search ->
        case query(trimmed_search) do
          {:ok, [result], "", _context, _line, _column} ->
            {:ok, result}

          {:ok, [result], rest, _context, _line, _column} ->
            {:error, "Unrecognized input `#{rest}`"}

          {:error, reason, _rest, _context, _line, _column} ->
            {:error, reason}
        end
    end
  end

  # ============================================================
  #  String
  # ============================================================

  spaces = repeat(ascii_char([?\s]))

  escaped_quote = ignore(ascii_char([?\\])) |> ascii_char([?"])

  escaped_backslash = ignore(ascii_char([?\\])) |> ascii_char([?\\])

  quoted_string =
    ignore(ascii_char([?"]))
    |> repeat(choice([escaped_quote, escaped_backslash, utf8_char([{:not, ?"}])]))
    |> ignore(ascii_char([?"]))
    |> reduce(:to_string)

  simple_string =
    times(
      ascii_char([
        {:not, ?"},
        {:not, ?!},
        {:not, ?\s},
        {:not, ?\t},
        {:not, ?\n},
        {:not, ?:},
        {:not, ?<},
        {:not, ?=},
        {:not, ?>},
        {:not, ?|},
        {:not, ?&},
        {:not, ?\(},
        {:not, ?\)}
      ]),
      min: 1
    )
    |> reduce(:to_string)

  any_string = choice([quoted_string, simple_string])

  # ============================================================
  #  Property
  # ============================================================

  defmodule PropertyHelper do
    def reduce_result(result, op) do
      [key | rest] = result
      [op_string | rest] = rest
      [value] = rest

      op =
        case op_string do
          ":" -> :match
          "<" -> :less
          "<=" -> :less_equal
          "=" -> :equal
          "==" -> :equal
          ">=" -> :greater_equal
          ">" -> :greater
        end

      {op, key, value}
    end
  end

  property_key = any_string

  property_value = any_string

  property_op =
    choice([
      string(":"),
      string("<="),
      string("<"),
      string("=="),
      string("="),
      string(">="),
      string(">")
    ])

  property_query =
    property_key
    |> ignore(spaces)
    |> concat(property_op)
    |> ignore(spaces)
    |> concat(property_value)
    |> reduce({PropertyHelper, :reduce_result, [:match]})

  # ============================================================
  #  Query
  # ============================================================

  defmodule QueryHelper do
    def reduce_keyword_query([keyword]), do: {:keyword, keyword}

    def reduce_not_query([query]), do: {:not, query}

    def reduce_logical_operator([operator]) do
      case operator do
        "&&" -> :and
        "||" -> :or
      end
    end

    def reduce_query(result) do
      IO.puts("Enter reduce_query with: #{inspect(result)}")

      chunks =
        result
        |> Enum.filter(fn x -> x != :and end)
        |> Enum.chunk_by(fn x -> x == :or end)

      IO.puts("Chunks are: #{inspect(chunks)}")

      if Enum.count(chunks) > 1 do
        # Has :or operator.
        queries =
          chunks
          |> Enum.filter(fn x -> x != [:or] end)
          |> Enum.map(&reduce_query/1)

        {:or, queries}
      else
        case hd(chunks) |> Enum.filter(fn x -> x != :and end) do
          [single] -> single
          many -> {:and, many}
        end
      end
    end
  end

  keyword_query = any_string |> reduce({QueryHelper, :reduce_keyword_query, []})

  single_query = choice([property_query, keyword_query])

  grouped_query =
    ignore(string("("))
    |> ignore(spaces)
    |> parsec(:query)
    |> ignore(spaces)
    |> ignore(string(")"))
    |> reduce({QueryHelper, :reduce_query, []})

  not_query =
    ignore(string("!"))
    |> ignore(spaces)
    |> choice([single_query, grouped_query])
    |> reduce({QueryHelper, :reduce_not_query, []})

  logical_operator =
    choice([string("&&"), string("||")]) |> reduce({QueryHelper, :reduce_logical_operator, []})

  query =
    repeat(
      choice([single_query, logical_operator, not_query, grouped_query])
      |> ignore(spaces)
    )
    |> reduce({QueryHelper, :reduce_query, []})

  defparsec(:query, query)
end
