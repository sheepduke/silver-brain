defmodule SilverBrain.Service.SqlSearchEngine do
  alias SilverBrain.Service.Schema

  import Ecto.Query

  @known_item_fields ["id", "name", "content_type", "content", "create_time", "update_time"]

  def search({:keyword, keyword}) when is_binary(keyword) do
    search({:match, "name", keyword})
  end

  # ============================================================
  #  Property Match
  # ============================================================

  def search({:match, key, value}) when is_binary(key) and is_binary(value) do
    search(
      fn key, value -> dynamic([item], like(field(item, ^key), ^value)) end,
      fn value -> dynamic([property], like(property.value, ^value)) end,
      fn value -> String.replace("%#{value}%", "*", "%") end
    ).(key, value)
  end

  def search({:less_than, key, value}) when is_binary(key) and is_binary(value) do
    search(
      fn key, value -> dynamic([item], field(item, ^key) < ^value) end,
      fn value -> dynamic([property], property.value < ^value) end
    ).(key, value)
  end

  def search({:less_equal, key, value}) when is_binary(key) and is_binary(value) do
    search(
      fn key, value -> dynamic([item], field(item, ^key) <= ^value) end,
      fn value -> dynamic([property], property.value <= ^value) end
    ).(key, value)
  end

  def search({:equal, key, value}) when is_binary(key) and is_binary(value) do
    search(
      fn key, value -> dynamic([item], field(item, ^key) == ^value) end,
      fn value -> dynamic([property], property.value == ^value) end
    ).(key, value)
  end

  def search({:not_equal, key, value}) when is_binary(key) and is_binary(value) do
    search(
      fn key, value -> dynamic([item], field(item, ^key) != ^value) end,
      fn value -> dynamic([property], property.value != ^value) end
    ).(key, value)
  end

  def search({:greater_than, key, value}) when is_binary(key) and is_binary(value) do
    search(
      fn key, value -> dynamic([item], field(item, ^key) > ^value) end,
      fn value -> dynamic([property], property.value > ^value) end
    ).(key, value)
  end

  def search({:greater_equal, key, value}) when is_binary(key) and is_binary(value) do
    search(
      fn key, value -> dynamic([item], field(item, ^key) >= ^value) end,
      fn value -> dynamic([property], property.value >= ^value) end
    ).(key, value)
  end

  # ============================================================
  #  Logical
  # ============================================================

  def search({:not, query}) do
    all_items_query = from(item in Schema.Item, select: [:id])

    except(all_items_query, ^search(query))
  end

  def search({:or, queries}) do
    queries
    |> Enum.map(&search(&1))
    |> Enum.reduce(fn x, acc -> union(x, ^acc) end)
  end

  def search({:and, queries}) do
    queries
    |> Enum.map(&search(&1))
    |> Enum.reduce(fn x, acc -> intersect(x, ^acc) end)
  end

  # ============================================================
  #  Internal
  # ============================================================

  defp search(
         item_where_fun,
         property_where_fun,
         value_map_fun \\ &Function.identity/1
       )
       when is_function(item_where_fun) and is_function(property_where_fun) and
              is_function(value_map_fun) do
    fn key, value ->
      key = key |> String.replace("-", "_") |> String.downcase()
      value = value_map_fun.(value)

      if Enum.member?(@known_item_fields, key) do
        # Given key is a known field.
        key = String.to_atom(key)

        from(item in Schema.Item, where: ^item_where_fun.(key, value), select: [:id])
      else
        # Given key is unknown, i.e., a custom field.
        from(property in Schema.ItemProperty,
          where: property.key == ^key and ^property_where_fun.(value),
          select: [:item_id]
        )
      end
    end
  end
end
