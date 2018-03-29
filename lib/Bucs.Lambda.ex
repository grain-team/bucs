# File: Bucs.Lambda.ex
# This file was generated from buclambda.beam
# Using rebar3_elixir (https://github.com/botsunit/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Bucs.Lambda do
  def unquote(:"curry")(arg1, arg2, arg3) do
    :erlang.apply(:"buclambda", :"curry", [arg1, arg2, arg3])
  end
  def unquote(:"curry")(arg1, arg2, arg3, arg4) do
    :erlang.apply(:"buclambda", :"curry", [arg1, arg2, arg3, arg4])
  end
  def unquote(:"rcurry")(arg1, arg2, arg3) do
    :erlang.apply(:"buclambda", :"rcurry", [arg1, arg2, arg3])
  end
  def unquote(:"rcurry")(arg1, arg2, arg3, arg4) do
    :erlang.apply(:"buclambda", :"rcurry", [arg1, arg2, arg3, arg4])
  end
end
