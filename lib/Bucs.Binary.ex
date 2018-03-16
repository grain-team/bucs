# File: Bucs.Binary.ex
# This file was generated from bucbinary.beam
# Using rebar3_elixir (https://github.com/botsunit/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Bucs.Binary do
  def unquote(:"rand_bits")(arg1) do
    :erlang.apply(:"bucbinary", :"rand_bits", [arg1])
  end
  def unquote(:"to_hexstr")(arg1) do
    :erlang.apply(:"bucbinary", :"to_hexstr", [arg1])
  end
  def unquote(:"from_hexstr")(arg1) do
    :erlang.apply(:"bucbinary", :"from_hexstr", [arg1])
  end
  def unquote(:"join")(arg1, arg2) do
    :erlang.apply(:"bucbinary", :"join", [arg1, arg2])
  end
  def unquote(:"trim")(arg1, arg2) do
    :erlang.apply(:"bucbinary", :"trim", [arg1, arg2])
  end
  def unquote(:"are_floats")(arg1) do
    :erlang.apply(:"bucbinary", :"are_floats", [arg1])
  end
  def unquote(:"is_float")(arg1) do
    :erlang.apply(:"bucbinary", :"is_float", [arg1])
  end
  def unquote(:"are_integers")(arg1) do
    :erlang.apply(:"bucbinary", :"are_integers", [arg1])
  end
  def unquote(:"is_integer")(arg1) do
    :erlang.apply(:"bucbinary", :"is_integer", [arg1])
  end
end
