defmodule SilverBrain.Core.SearchQueryTest do
  use ExUnit.Case, async: true

  import SilverBrain.Core.SearchQuery, only: [parse: 1]

  test "parse empty string" do
    assert parse("") == nil
  end

  test "parse simple string" do
    text = "qwertyuiopasdfghjklzxcvbnm@#$%^*"
    assert parse(text) == {:ok, {:keyword, text}}
  end

  test "parse quoted string" do
    assert parse("\"aa\\\\bb\\\"cc\\\\\\\"dd\"") == {:ok, {:keyword, "aa\\bb\"cc\\\"dd"}}
  end

  test "parse property" do
    assert parse("key: value") == {:ok, {:match, "key", "value"}}
    assert parse("key == value") == {:ok, {:equal, "key", "value"}}
    assert parse("key = value") == {:ok, {:equal, "key", "value"}}
    assert parse("key < value") == {:ok, {:less, "key", "value"}}
    assert parse("key <= value") == {:ok, {:less_equal, "key", "value"}}
    assert parse("key >= value") == {:ok, {:greater_equal, "key", "value"}}
    assert parse("key > value") == {:ok, {:greater, "key", "value"}}
  end

  test "parse combinations" do
    assert parse("aa && bb || cc dd") ==
             {:ok,
              {:or,
               [
                 {:and, [{:keyword, "aa"}, {:keyword, "bb"}]},
                 {:and, [{:keyword, "cc"}, {:keyword, "dd"}]}
               ]}}

    assert parse("aa && (bb || cc) dd") ==
             {:ok,
              {:and,
               [
                 {:keyword, "aa"},
                 {:or, [{:keyword, "bb"}, {:keyword, "cc"}]},
                 {:keyword, "dd"}
               ]}}

    assert parse("!  aa && !(bb || !cc)") ==
             {:ok,
              {:and,
               [
                 {:not, {:keyword, "aa"}},
                 {:not, {:or, [{:keyword, "bb"}, {:not, {:keyword, "cc"}}]}}
               ]}}
  end
end
