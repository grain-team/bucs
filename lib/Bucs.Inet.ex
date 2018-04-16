# File: Bucs.Inet.ex
# This file was generated from bucinet.beam
# Using rebar3_elixir (https://github.com/G-Corp/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Bucs.Inet do
  def unquote(:"to_ip")(arg1) do
    :erlang.apply(:"bucinet", :"to_ip", [arg1])
  end
  def unquote(:"ip_to_string")(arg1) do
    :erlang.apply(:"bucinet", :"ip_to_string", [arg1])
  end
  def unquote(:"ip_to_binary")(arg1) do
    :erlang.apply(:"bucinet", :"ip_to_binary", [arg1])
  end
  def unquote(:"active_ip")() do
    :erlang.apply(:"bucinet", :"active_ip", [])
  end
  def unquote(:"active_ips")() do
    :erlang.apply(:"bucinet", :"active_ips", [])
  end
  def unquote(:"loopback")() do
    :erlang.apply(:"bucinet", :"loopback", [])
  end
  def unquote(:"is_ip")(arg1) do
    :erlang.apply(:"bucinet", :"is_ip", [arg1])
  end
  def unquote(:"country")(arg1) do
    :erlang.apply(:"bucinet", :"country", [arg1])
  end
  def unquote(:"country")(arg1, arg2) do
    :erlang.apply(:"bucinet", :"country", [arg1, arg2])
  end
end
