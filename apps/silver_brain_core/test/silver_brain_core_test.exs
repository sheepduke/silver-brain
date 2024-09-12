defmodule SilverBrainCoreTest do
  use ExUnit.Case
  doctest SilverBrainCore

  test "greets the world" do
    assert SilverBrainCore.hello() == :world
  end
end
