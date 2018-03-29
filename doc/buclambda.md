

# Module buclambda #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#curry-3">curry/3</a></td><td> 
Returns a curried equivalent of the provided function.</td></tr><tr><td valign="top"><a href="#curry-4">curry/4</a></td><td> 
Returns a curried equivalent of the provided function, with the specified deep or params.</td></tr><tr><td valign="top"><a href="#rcurry-3">rcurry/3</a></td><td>
Returns a <i>reverse</i> curried equivalent of the provided function.</td></tr><tr><td valign="top"><a href="#rcurry-4">rcurry/4</a></td><td>
Returns a <i>reverse</i> curried equivalent of the provided function, with the specified deep or params.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="curry-3"></a>

### curry/3 ###

<pre><code>
curry(Module::module(), Function::atom(), Arity::integer()) -&gt; {ok, function()} | error
</code></pre>
<br />


Returns a curried equivalent of the provided function.

```erlang

 curry:curry(lists, keystore, 4).
 % => {ok,#Fun<erl_eval.6.99386804>}
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

 curry:curry(lists, keystore, 4, 4).
 % => {ok,#Fun<erl_eval.6.99386804>}
 % => fun(A0) -> fun(A1) -> fun(A2) -> fun(A3) -> lists:keystore(A0, A1, A2, A3) end end end end.

 curry:curry(lists, keystore, 4, 3).
 % => {ok,#Fun<erl_eval.12.99386804>}
 % => fun(A0, A1) -> fun(A2) -> fun(A3) -> lists:keystore(A0, A1, A2, A3) end end end.

 curry:curry(lists, keystore, 4, 2).
 % =>{ok,#Fun<erl_eval.18.99386804>}
 % => fun(A0, A1, A2) -> fun(A3) -> lists:keystore(A0, A1, A2, A3) end end.

 curry:curry(lists, keystore, 4, 1).
 % => {ok,#Fun<erl_eval.4.99386804>}
 % => fun(A0, A1, A2, A3) -> lists:keystore(A0, A1, A2, A3) end.

 {ok, F} = curry:curry(lists, keyfind, 3, [toto, 1]).
 F([{tata, 1}, {toto, 2}, {titi, 3}]).
 % => {toto,2}
 F([{tata, 1}, {tutu, 2}, {titi, 3}]).
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

 curry:rcurry(lists, keystore, 4).
 % => {ok,#Fun<erl_eval.6.99386804>}
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

 curry:rcurry(lists, keystore, 4, 4).
 % => {ok,#Fun<erl_eval.6.99386804>}
 % => fun(A3) -> fun(A2) -> fun(A1) -> fun(A0) -> lists:keystore(A0, A1, A2, A3) end end end end.

 curry:rcurry(lists, keystore, 4, 3).
 % => {ok,#Fun<erl_eval.12.99386804>}
 % => fun(A3, A2) -> fun(A1) -> fun(A0) -> lists:keystore(A0, A1, A2, A3) end end end.

 curry:rcurry(lists, keystore, 4, 2).
 % => {ok,#Fun<erl_eval.18.99386804>}
 % => fun(A3, A2, A1) -> fun(A0) -> lists:keystore(A0, A1, A2, A3) end end.

 curry:rcurry(lists, keystore, 4, 1).
 % => {ok,#Fun<erl_eval.4.99386804>}
 % => fun(A3, A2, A1, A0) -> lists:keystore(A0, A1, A2, A3) end.

 {ok, F} = curry:rcurry(lists, keyfind, 3, [[{tata, 1}, {toto, 2}, {titi, 3}], 1]).
 F(toto).
 % => {toto,2}
 F(tutu).
 % => false
```

