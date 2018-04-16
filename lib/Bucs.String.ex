# File: Bucs.String.ex
# This file was generated from bucstring.beam
# Using rebar3_elixir (https://github.com/G-Corp/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Bucs.String do
  def unquote(:"sub")(arg1, arg2, arg3) do
    :erlang.apply(:"bucstring", :"sub", [arg1, arg2, arg3])
  end
  def unquote(:"lowercase")(arg1) do
    :erlang.apply(:"bucstring", :"lowercase", [arg1])
  end
  def unquote(:"uppercase")(arg1) do
    :erlang.apply(:"bucstring", :"uppercase", [arg1])
  end
  def unquote(:"split")(arg1, arg2) do
    :erlang.apply(:"bucstring", :"split", [arg1, arg2])
  end
  def unquote(:"gsub")(arg1, arg2, arg3) do
    :erlang.apply(:"bucstring", :"gsub", [arg1, arg2, arg3])
  end
end
