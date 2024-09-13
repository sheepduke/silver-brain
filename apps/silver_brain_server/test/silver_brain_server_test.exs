defmodule SilverBrainServerTest do
  use ExUnit.Case
  doctest SilverBrainServer

  test "greets the world" do
    assert SilverBrainServer.hello() == :world
  end
end
