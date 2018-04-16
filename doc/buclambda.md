

# Module buclambda #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#compose-1">compose/1</a></td><td> 
Performs right-to-left function composition.</td></tr><tr><td valign="top"><a href="#curry-1">curry/1</a></td><td> 
Returns a curried equivalent of the provided function.</td></tr><tr><td valign="top"><a href="#curry-2">curry/2</a></td><td> 
Returns a curried equivalent of the provided function, with the specified deep or params.</td></tr><tr><td valign="top"><a href="#curry-3">curry/3</a></td><td> 
Returns a curried equivalent of the provided function.</td></tr><tr><td valign="top"><a href="#curry-4">curry/4</a></td><td> 
Returns a curried equivalent of the provided function, with the specified deep or params.</td></tr><tr><td valign="top"><a href="#f_curry-1">f_curry/1</a></td><td>
Same as <tt>buclambda:curry/1</tt> but fails with an exception on error.</td></tr><tr><td valign="top"><a href="#f_curry-2">f_curry/2</a></td><td>
Same as <tt>buclambda:curry/2</tt> but fails with an exception on error.</td></tr><tr><td valign="top"><a href="#f_curry-3">f_curry/3</a></td><td>
Same as <tt>buclambda:curry/3</tt> but fails with an exception on error.</td></tr><tr><td valign="top"><a href="#f_curry-4">f_curry/4</a></td><td>
Same as <tt>buclambda:curry/4</tt> but fails with an exception on error.</td></tr><tr><td valign="top"><a href="#f_rcurry-1">f_rcurry/1</a></td><td>
Same as <tt>buclambda:r_curry/1</tt> but fails with an exception on error.</td></tr><tr><td valign="top"><a href="#f_rcurry-2">f_rcurry/2</a></td><td>
Same as <tt>buclambda:rcurry/2</tt> but fails with an exception on error.</td></tr><tr><td valign="top"><a href="#f_rcurry-3">f_rcurry/3</a></td><td>
Same as <tt>buclambda:rcurry/3</tt> but fails with an exception on error.</td></tr><tr><td valign="top"><a href="#f_rcurry-4">f_rcurry/4</a></td><td>
Same as <tt>buclambda:rcurry/4</tt> but fails with an exception on error.</td></tr><tr><td valign="top"><a href="#pipe-1">pipe/1</a></td><td> 
Performs left-to-right function composition.</td></tr><tr><td valign="top"><a href="#rcurry-1">rcurry/1</a></td><td>
Returns a <i>reverse</i> curried equivalent of the provided function.</td></tr><tr><td valign="top"><a href="#rcurry-2">rcurry/2</a></td><td>
Returns a <i>reverse</i> curried equivalent of the provided function, with the specified deep or params.</td></tr><tr><td valign="top"><a href="#rcurry-3">rcurry/3</a></td><td>
Returns a <i>reverse</i> curried equivalent of the provided function.</td></tr><tr><td valign="top"><a href="#rcurry-4">rcurry/4</a></td><td>
Returns a <i>reverse</i> curried equivalent of the provided function, with the specified deep or params.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="compose-1"></a>

### compose/1 ###

<pre><code>
compose(Functions::[function()]) -&gt; {ok, function()} | error
</code></pre>
<br />


Performs right-to-left function composition.

```erlang

 {ok, F} = buclambda:compose([
   fun erlang:length/1,
   buclambda:f_curry(fun lists:filter/2, [fun(E) -> E > 10 end])
 ]).
 F([8, 9, 10, 11, 12]).
 % => 2
```

<a name="curry-1"></a>

### curry/1 ###

<pre><code>
curry(Fun::function()) -&gt; {ok, function()} | error
</code></pre>
<br />


Returns a curried equivalent of the provided function.

```erlang

 buclambda:curry(fun lists:keystore/4).
 % => {ok, #Fun<erl_eval.6.99386804>}
 % => fun(A0) -> fun(A1) -> fun(A2) -> fun(A3) -> lists:keystore(A0, A1, A2, A3) end end end end.
```

<a name="curry-2"></a>

### curry/2 ###

<pre><code>
curry(Fun::function(), DeepOrArgs::integer() | [term()]) -&gt; {ok, function()} | error
</code></pre>
<br />


Returns a curried equivalent of the provided function, with the specified deep or params.

```erlang

 buclambda:curry(fun lists:keystore/4, 4).
 % => {ok, #Fun<erl_eval.6.99386804>}
 % => fun(A0) -> fun(A1) -> fun(A2) -> fun(A3) -> lists:keystore(A0, A1, A2, A3) end end end end.

 buclambda:curry(fun lists:keystore/4, 3).
 % => {ok, #Fun<erl_eval.12.99386804>}
 % => fun(A0, A1) -> fun(A2) -> fun(A3) -> lists:keystore(A0, A1, A2, A3) end end end.

 buclambda:curry(fun lists:keystore/4, 2).
 % =>{ok, #Fun<erl_eval.18.99386804>}
 % => fun(A0, A1, A2) -> fun(A3) -> lists:keystore(A0, A1, A2, A3) end end.

 buclambda:curry(fun lists:keystore/4, 1).
 % => {ok, #Fun<erl_eval.4.99386804>}
 % => fun(A0, A1, A2, A3) -> lists:keystore(A0, A1, A2, A3) end.

 {ok, F} = buclambda:curry(fun lists:keyfind/3, [toto, 1]).
 F([{tata, 1}, {toto, 2}, {titi, 3}]).
 % => {toto, 2}
 F([{tata, 1}, {tutu, 2}, {titi, 3}]).
 % => false
```

<a name="curry-3"></a>

### curry/3 ###

<pre><code>
curry(Module::module(), Function::atom(), Arity::integer()) -&gt; {ok, function()} | error
</code></pre>
<br />


Returns a curried equivalent of the provided function.

```erlang

 buclambda:curry(lists, keystore, 4).
 % => {ok, #Fun<erl_eval.6.99386804>}
 % => fun(A0) -> fun(A1) -> fun(A2) -> fun(A3) -> lists:keystore(A0, A1, A2, A3) end end end end.
```

<a name="curry-4"></a>

### curry/4 ###

<pre><code>
curry(Module::module(), Function::atom(), Arity::integer(), DeepOrArgs::integer() | [term()]) -&gt; {ok, function()} | error
</code></pre>
<br />


Returns a curried equivalent of the provided function, with the specified deep or params.

```erlang

 buclambda:curry(lists, keystore, 4, 4).
 % => {ok, #Fun<erl_eval.6.99386804>}
 % => fun(A0) -> fun(A1) -> fun(A2) -> fun(A3) -> lists:keystore(A0, A1, A2, A3) end end end end.

 buclambda:curry(lists, keystore, 4, 3).
 % => {ok, #Fun<erl_eval.12.99386804>}
 % => fun(A0, A1) -> fun(A2) -> fun(A3) -> lists:keystore(A0, A1, A2, A3) end end end.

 buclambda:curry(lists, keystore, 4, 2).
 % =>{ok, #Fun<erl_eval.18.99386804>}
 % => fun(A0, A1, A2) -> fun(A3) -> lists:keystore(A0, A1, A2, A3) end end.

 buclambda:curry(lists, keystore, 4, 1).
 % => {ok, #Fun<erl_eval.4.99386804>}
 % => fun(A0, A1, A2, A3) -> lists:keystore(A0, A1, A2, A3) end.

 {ok, F} = buclambda:curry(lists, keyfind, 3, [toto, 1]).
 F([{tata, 1}, {toto, 2}, {titi, 3}]).
 % => {toto, 2}
 F([{tata, 1}, {tutu, 2}, {titi, 3}]).
 % => false
```

<a name="f_curry-1"></a>

### f_curry/1 ###

<pre><code>
f_curry(Fun::function()) -&gt; function()
</code></pre>
<br />

Same as `buclambda:curry/1` but fails with an exception on error

<a name="f_curry-2"></a>

### f_curry/2 ###

<pre><code>
f_curry(Fun::function(), DeepOrArgs::integer() | [term()]) -&gt; function()
</code></pre>
<br />

Same as `buclambda:curry/2` but fails with an exception on error

<a name="f_curry-3"></a>

### f_curry/3 ###

<pre><code>
f_curry(Module::module(), Function::atom(), Arity::integer()) -&gt; function()
</code></pre>
<br />

Same as `buclambda:curry/3` but fails with an exception on error

<a name="f_curry-4"></a>

### f_curry/4 ###

<pre><code>
f_curry(Module::module(), Function::atom(), Arity::integer(), DeepOrArgs::integer() | [term()]) -&gt; function()
</code></pre>
<br />

Same as `buclambda:curry/4` but fails with an exception on error

<a name="f_rcurry-1"></a>

### f_rcurry/1 ###

<pre><code>
f_rcurry(Fun::function()) -&gt; function()
</code></pre>
<br />

Same as `buclambda:r_curry/1` but fails with an exception on error

<a name="f_rcurry-2"></a>

### f_rcurry/2 ###

<pre><code>
f_rcurry(Fun::function(), DeepOrArgs::integer() | [term()]) -&gt; function()
</code></pre>
<br />

Same as `buclambda:rcurry/2` but fails with an exception on error

<a name="f_rcurry-3"></a>

### f_rcurry/3 ###

<pre><code>
f_rcurry(Module::module(), Function::atom(), Arity::integer()) -&gt; function()
</code></pre>
<br />

Same as `buclambda:rcurry/3` but fails with an exception on error

<a name="f_rcurry-4"></a>

### f_rcurry/4 ###

<pre><code>
f_rcurry(Module::module(), Function::atom(), Arity::integer(), DeepOrArgs::integer() | [term()]) -&gt; function()
</code></pre>
<br />

Same as `buclambda:rcurry/4` but fails with an exception on error

<a name="pipe-1"></a>

### pipe/1 ###

<pre><code>
pipe(Functions::[function() | {function(), [term()]}]) -&gt; {ok, function()} | error
</code></pre>
<br />


Performs left-to-right function composition.

```erlang

 {ok, F} = buclambda:pipe([
   buclambda:f_curry(fun lists:filter/2, [fun(E) -> E > 10 end]),
   fun erlang:length/1
 ]).
 F([8, 9, 10, 11, 12]).
 % => 2
```

<a name="rcurry-1"></a>

### rcurry/1 ###

<pre><code>
rcurry(Fun::function()) -&gt; {ok, function()} | error
</code></pre>
<br />

Returns a _reverse_ curried equivalent of the provided function.

```erlang

 buclambda:rcurry(fun lists:keystore/4).
 % => {ok, #Fun<erl_eval.6.99386804>}
 % => fun(A3) -> fun(A2) -> fun(A1) -> fun(A0) -> lists:keystore(A0, A1, A2, A3) end end end end.
```

<a name="rcurry-2"></a>

### rcurry/2 ###

<pre><code>
rcurry(Fun::function(), DeepOrArgs::integer() | [term()]) -&gt; {ok, function()} | error
</code></pre>
<br />

Returns a _reverse_ curried equivalent of the provided function, with the specified deep or params.

```erlang

 buclambda:rcurry(fun lists:keystore/4, 4).
 % => {ok, #Fun<erl_eval.6.99386804>}
 % => fun(A3) -> fun(A2) -> fun(A1) -> fun(A0) -> lists:keystore(A0, A1, A2, A3) end end end end.

 buclambda:rcurry(fun lists:keystore/4, 3).
 % => {ok, #Fun<erl_eval.12.99386804>}
 % => fun(A3, A2) -> fun(A1) -> fun(A0) -> lists:keystore(A0, A1, A2, A3) end end end.

 buclambda:rcurry(fun lists:keystore/4, 2).
 % => {ok, #Fun<erl_eval.18.99386804>}
 % => fun(A3, A2, A1) -> fun(A0) -> lists:keystore(A0, A1, A2, A3) end end.

 buclambda:rcurry(fun lists:keystore/4, 1).
 % => {ok, #Fun<erl_eval.4.99386804>}
 % => fun(A3, A2, A1, A0) -> lists:keystore(A0, A1, A2, A3) end.

 {ok, F} = buclambda:rcurry(lists, keyfind, 3, [[{tata, 1}, {toto, 2}, {titi, 3}], 1]).
 F(toto).
 % => {toto, 2}
 F(tutu).
 % => false
```

<a name="rcurry-3"></a>

### rcurry/3 ###

<pre><code>
rcurry(Module::module(), Function::atom(), Arity::integer()) -&gt; {ok, function()} | error
</code></pre>
<br />

Returns a _reverse_ curried equivalent of the provided function.

```erlang

 buclambda:rcurry(lists, keystore, 4).
 % => {ok, #Fun<erl_eval.6.99386804>}
 % => fun(A3) -> fun(A2) -> fun(A1) -> fun(A0) -> lists:keystore(A0, A1, A2, A3) end end end end.
```

<a name="rcurry-4"></a>

### rcurry/4 ###

<pre><code>
rcurry(Module::module(), Function::atom(), Arity::integer(), DeepOrArgs::integer() | [term()]) -&gt; {ok, function()} | error
</code></pre>
<br />

Returns a _reverse_ curried equivalent of the provided function, with the specified deep or params.

```erlang

 buclambda:rcurry(lists, keystore, 4, 4).
 % => {ok, #Fun<erl_eval.6.99386804>}
 % => fun(A3) -> fun(A2) -> fun(A1) -> fun(A0) -> lists:keystore(A0, A1, A2, A3) end end end end.

 buclambda:rcurry(lists, keystore, 4, 3).
 % => {ok, #Fun<erl_eval.12.99386804>}
 % => fun(A3, A2) -> fun(A1) -> fun(A0) -> lists:keystore(A0, A1, A2, A3) end end end.

 buclambda:rcurry(lists, keystore, 4, 2).
 % => {ok, #Fun<erl_eval.18.99386804>}
 % => fun(A3, A2, A1) -> fun(A0) -> lists:keystore(A0, A1, A2, A3) end end.

 buclambda:rcurry(lists, keystore, 4, 1).
 % => {ok, #Fun<erl_eval.4.99386804>}
 % => fun(A3, A2, A1, A0) -> lists:keystore(A0, A1, A2, A3) end.

 {ok, F} = buclambda:rcurry(lists, keyfind, 3, [[{tata, 1}, {toto, 2}, {titi, 3}], 1]).
 F(toto).
 % => {toto, 2}
 F(tutu).
 % => false
```

