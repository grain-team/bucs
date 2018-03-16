# File: Bucs.Timer.ex
# This file was generated from buctimer.beam
# Using rebar3_elixir (https://github.com/botsunit/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Bucs.Timer do
  def unquote(:"verify")(arg1) do
    :erlang.apply(:"buctimer", :"verify", [arg1])
  end
  def unquote(:"next")(arg1) do
    :erlang.apply(:"buctimer", :"next", [arg1])
  end
  def unquote(:"next")(arg1, arg2) do
    :erlang.apply(:"buctimer", :"next", [arg1, arg2])
  end
end
