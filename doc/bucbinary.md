

# Module bucbinary #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#are_floats-1">are_floats/1</a></td><td>
Return true if all binaries in the given list can be converted to float ; false otherwise.</td></tr><tr><td valign="top"><a href="#are_integers-1">are_integers/1</a></td><td>
Return true if all binaries in the given list can be converted to integer ; false otherwise.</td></tr><tr><td valign="top"><a href="#from_hexstr-1">from_hexstr/1</a></td><td>
Convert a Hex string to binary.</td></tr><tr><td valign="top"><a href="#is_float-1">is_float/1</a></td><td>
Return true if the given binary can be converted to float ; false otherwise.</td></tr><tr><td valign="top"><a href="#is_integer-1">is_integer/1</a></td><td>
Return true if the given binary can be converted to integer ; false otherwise.</td></tr><tr><td valign="top"><a href="#join-2">join/2</a></td><td>
join a list of binaries with the given separator.</td></tr><tr><td valign="top"><a href="#rand_bits-1">rand_bits/1</a></td><td>
Generate random binary.</td></tr><tr><td valign="top"><a href="#to_hexstr-1">to_hexstr/1</a></td><td>
Convert a binary to and Hex string.</td></tr><tr><td valign="top"><a href="#trim-2">trim/2</a></td><td>
Trim a binary.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="are_floats-1"></a>

### are_floats/1 ###

<pre><code>
are_floats(Binaries::[binary()]) -&gt; true | false
</code></pre>
<br />

Return true if all binaries in the given list can be converted to float ; false otherwise

<a name="are_integers-1"></a>

### are_integers/1 ###

<pre><code>
are_integers(Binaries::[binary()]) -&gt; true | false
</code></pre>
<br />

Return true if all binaries in the given list can be converted to integer ; false otherwise

<a name="from_hexstr-1"></a>

### from_hexstr/1 ###

<pre><code>
from_hexstr(String::list()) -&gt; binary()
</code></pre>
<br />

Convert a Hex string to binary

<a name="is_float-1"></a>

### is_float/1 ###

<pre><code>
is_float(Binary::binary()) -&gt; true | false
</code></pre>
<br />

Return true if the given binary can be converted to float ; false otherwise

<a name="is_integer-1"></a>

### is_integer/1 ###

<pre><code>
is_integer(Binary::binary()) -&gt; true | false
</code></pre>
<br />

Return true if the given binary can be converted to integer ; false otherwise

<a name="join-2"></a>

### join/2 ###

<pre><code>
join(L::[term()], S::binary()) -&gt; binary()
</code></pre>
<br />

join a list of binaries with the given separator

Example:

```erlang

  <<"toto-tata-titi">> = bucbinary:join([<<"toto">>, <<"tata">>, <<"titi">>], <<"-">>).
```

<a name="rand_bits-1"></a>

### rand_bits/1 ###

<pre><code>
rand_bits(Bits::non_neg_integer()) -&gt; binary()
</code></pre>
<br />

Generate random binary

<a name="to_hexstr-1"></a>

### to_hexstr/1 ###

<pre><code>
to_hexstr(Bin::binary()) -&gt; list()
</code></pre>
<br />

Convert a binary to and Hex string

<a name="trim-2"></a>

### trim/2 ###

<pre><code>
trim(Binary::binary(), Direction::left | right | both) -&gt; binary()
</code></pre>
<br />

Trim a binary

