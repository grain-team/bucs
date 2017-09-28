

# Module bucbinary #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#are_floats-1">are_floats/1</a></td><td></td></tr><tr><td valign="top"><a href="#are_integers-1">are_integers/1</a></td><td></td></tr><tr><td valign="top"><a href="#from_hexstr-1">from_hexstr/1</a></td><td>
Convert a Hex string to binary.</td></tr><tr><td valign="top"><a href="#is_float-1">is_float/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_integer-1">is_integer/1</a></td><td></td></tr><tr><td valign="top"><a href="#join-2">join/2</a></td><td>
join a list of binaries with the given separator.</td></tr><tr><td valign="top"><a href="#rand_bits-1">rand_bits/1</a></td><td>
Generate random binary.</td></tr><tr><td valign="top"><a href="#to_hexstr-1">to_hexstr/1</a></td><td>
Convert a binary to and Hex string.</td></tr><tr><td valign="top"><a href="#trim-2">trim/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="are_floats-1"></a>

### are_floats/1 ###

<pre><code>
are_floats(List::[binary()]) -&gt; true | false
</code></pre>
<br />

<a name="are_integers-1"></a>

### are_integers/1 ###

<pre><code>
are_integers(List::[binary()]) -&gt; true | false
</code></pre>
<br />

<a name="from_hexstr-1"></a>

### from_hexstr/1 ###

`from_hexstr(S) -> any()`

Convert a Hex string to binary

<a name="is_float-1"></a>

### is_float/1 ###

<pre><code>
is_float(Data::binary()) -&gt; true | false
</code></pre>
<br />

<a name="is_integer-1"></a>

### is_integer/1 ###

<pre><code>
is_integer(Data::binary()) -&gt; true | false
</code></pre>
<br />

<a name="join-2"></a>

### join/2 ###

<pre><code>
join(L::[binary()], S::binary()) -&gt; binary()
</code></pre>
<br />

join a list of binaries with the given separator

Example:

```erlang

  <<"toto-tata-titi">> = bucbinary:join([<<"toto">>, <<"tata">>, <<"titi">>], <<"-">>).
```

<a name="rand_bits-1"></a>

### rand_bits/1 ###

`rand_bits(Bits) -> any()`

Generate random binary

<a name="to_hexstr-1"></a>

### to_hexstr/1 ###

`to_hexstr(Bin) -> any()`

Convert a binary to and Hex string

<a name="trim-2"></a>

### trim/2 ###

<pre><code>
trim(Binary::binary(), X2::left | right | both) -&gt; binary()
</code></pre>
<br />

