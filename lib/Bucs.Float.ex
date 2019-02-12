# File: Bucs.Float.ex
# This file was generated from bucfloat.beam
# Using rebar3_elixir (https://github.com/G-Corp/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Bucs.Float do
  def unquote(:round)(arg1, arg2) do
    :erlang.apply(:bucfloat, :round, [arg1, arg2])
  end
end
