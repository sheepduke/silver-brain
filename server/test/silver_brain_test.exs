defmodule SilverBrainTest do
  use ExUnit.Case
  doctest SilverBrain

  test "greets the world" do
    assert SilverBrain.hello() == :world
  end
end
