

# Module bucs #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-type">type()</a> ###


<pre><code>
type() = binary | list | string | atom | float | integer | pid | reference | port | tuple | map | function | boolean
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#apply-2">apply/2</a></td><td>
Execute the given function and return its result.</td></tr><tr><td valign="top"><a href="#apply-3">apply/3</a></td><td>
Execute the given function and return the result or <tt>Default</tt> if the function
does not exists.</td></tr><tr><td valign="top"><a href="#apply-4">apply/4</a></td><td>
Execute the given function and return the result or <tt>Default</tt> if the function
does not exists.</td></tr><tr><td valign="top"><a href="#as-2">as/2</a></td><td>
Return <tt>Data</tt> in the same type than <tt>Type</tt>.</td></tr><tr><td valign="top"><a href="#blank-1">blank/1</a></td><td>
Return true if <tt>Data</tt> is <i>blank</i></td></tr><tr><td valign="top"><a href="#call-3">call/3</a></td><td> 
Returns the result of applying Function in Module to Args.</td></tr><tr><td valign="top"><a href="#compare_as_atom-2">compare_as_atom/2</a></td><td>
Compare <tt>A</tt> and <tt>B</tt> as if they were atoms.</td></tr><tr><td valign="top"><a href="#compare_as_binary-2">compare_as_binary/2</a></td><td>
Compare <tt>A</tt> and <tt>B</tt> as if they were binaries.</td></tr><tr><td valign="top"><a href="#compare_as_float-2">compare_as_float/2</a></td><td>
Compare <tt>A</tt> and <tt>B</tt> as if they were floats.</td></tr><tr><td valign="top"><a href="#compare_as_integer-2">compare_as_integer/2</a></td><td>
Compare <tt>A</tt> and <tt>B</tt> as if they were integers.</td></tr><tr><td valign="top"><a href="#compare_as_list-2">compare_as_list/2</a></td><td>
Compare <tt>A</tt> and <tt>B</tt> as if they were lists.</td></tr><tr><td valign="top"><a href="#compare_as_string-2">compare_as_string/2</a></td><td>
Compare <tt>A</tt> and <tt>B</tt> as if they were strings.</td></tr><tr><td valign="top"><a href="#default_to-2">default_to/2</a></td><td>
Return <tt>Default</tt> if <tt>Data</tt> is black, <tt>Data</tt> otherwise.</td></tr><tr><td valign="top"><a href="#eval-1">eval/1</a></td><td>Equivalent to <a href="#eval-2"><tt>eval(Expression, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#eval-2">eval/2</a></td><td>
Evaluate the given <tt>Expression</tt> with the given <tt>Environment</tt>.</td></tr><tr><td valign="top"><a href="#function_exist-3">function_exist/3</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#function_exists-3">function_exists/3</a></td><td>
Check if the given function exist.</td></tr><tr><td valign="top"><a href="#is_kw_list-1">is_kw_list/1</a></td><td>
Check if the given value is a keyword list.</td></tr><tr><td valign="top"><a href="#is_list_of-2">is_list_of/2</a></td><td>
Return true if all elements in the <tt>List</tt> are of type <tt>Type</tt>.</td></tr><tr><td valign="top"><a href="#is_list_of_lists-1">is_list_of_lists/1</a></td><td>
Check if the given value is a list of lists.</td></tr><tr><td valign="top"><a href="#is_string-1">is_string/1</a></td><td>
Check if the given value is a string.</td></tr><tr><td valign="top"><a href="#is_tuple_of-2">is_tuple_of/2</a></td><td>
Return true if <tt>Tuple</tt> is a tuple matching the tuple <tt>Pattern</tt></td></tr><tr><td valign="top"><a href="#is_type-2">is_type/2</a></td><td>
Return true if <tt>Data</tt> is of type <tt>Type</tt></td></tr><tr><td valign="top"><a href="#match-2">match/2</a></td><td>
Return true if <tt>A</tt> match <tt>B</tt>.</td></tr><tr><td valign="top"><a href="#module_exist-1">module_exist/1</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#module_exists-1">module_exists/1</a></td><td>
Check if the given module exist.</td></tr><tr><td valign="top"><a href="#pipecall-1">pipecall/1</a></td><td> 
Pipe fun call.</td></tr><tr><td valign="top"><a href="#present-1">present/1</a></td><td>
Return true if <tt>Data</tt> is <i>not blank</i></td></tr><tr><td valign="top"><a href="#to-2">to/2</a></td><td>
Convert <tt>Data</tt> in type <tt>Type</tt>.</td></tr><tr><td valign="top"><a href="#to_atom-1">to_atom/1</a></td><td> 
Convert the given term to atom.</td></tr><tr><td valign="top"><a href="#to_binary-1">to_binary/1</a></td><td> 
Convert the given term to binary.</td></tr><tr><td valign="top"><a href="#to_float-1">to_float/1</a></td><td> 
Convert the given term to float.</td></tr><tr><td valign="top"><a href="#to_float-2">to_float/2</a></td><td> 
Convert the given term to float, with the given precision.</td></tr><tr><td valign="top"><a href="#to_integer-1">to_integer/1</a></td><td> 
Convert the given term to integer.</td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td> 
Convert the given term to list.</td></tr><tr><td valign="top"><a href="#to_string-1">to_string/1</a></td><td>
Convert the given term to string.</td></tr><tr><td valign="top"><a href="#to_term-1">to_term/1</a></td><td> 
Convert the given value to term.</td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td>Return the type (atom, string, float, ...) of <tt>Data</tt></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="apply-2"></a>

### apply/2 ###

<pre><code>
apply(Fun::function(), Args::[term()]) -&gt; {ok, term()} | error
</code></pre>
<br />

Execute the given function and return its result.

<a name="apply-3"></a>

### apply/3 ###

<pre><code>
apply(FunOrModule::function() | module(), ArgsOrFunction::[term()] | atom(), DefaultOrArgs::term() | [term()]) -&gt; {ok, term() | error}
</code></pre>
<br />

Execute the given function and return the result or `Default` if the function
does not exists

<a name="apply-4"></a>

### apply/4 ###

<pre><code>
apply(Module::module(), Function::atom(), Args::[term()], Default::term()) -&gt; term()
</code></pre>
<br />

Execute the given function and return the result or `Default` if the function
does not exists

<a name="as-2"></a>

### as/2 ###

<pre><code>
as(Type::term(), Data::term()) -&gt; term()
</code></pre>
<br />

Return `Data` in the same type than `Type`.

```

 bucs:as("this is a string", <<"this is a binary">>).
 % => "this is a binary"
```

<a name="blank-1"></a>

### blank/1 ###

<pre><code>
blank(Data::term()) -&gt; true | false
</code></pre>
<br />

Return true if `Data` is _blank_

<a name="call-3"></a>

### call/3 ###

<pre><code>
call(Module::module(), Function::atom(), Args::[term()]) -&gt; term() | {error, undefined_function}
</code></pre>
<br />


Returns the result of applying Function in Module to Args. The applied 
function must be exported from Module. The arity of the function is the length of Args.

Return `{error, undefined_function}` if the applied function is not exported.

<a name="compare_as_atom-2"></a>

### compare_as_atom/2 ###

<pre><code>
compare_as_atom(A::term(), B::term()) -&gt; true | false
</code></pre>
<br />

Compare `A` and `B` as if they were atoms

<a name="compare_as_binary-2"></a>

### compare_as_binary/2 ###

<pre><code>
compare_as_binary(A::term(), B::term()) -&gt; true | false
</code></pre>
<br />

Compare `A` and `B` as if they were binaries

<a name="compare_as_float-2"></a>

### compare_as_float/2 ###

<pre><code>
compare_as_float(A::term(), B::term()) -&gt; true | false
</code></pre>
<br />

Compare `A` and `B` as if they were floats

<a name="compare_as_integer-2"></a>

### compare_as_integer/2 ###

<pre><code>
compare_as_integer(A::term(), B::term()) -&gt; true | false
</code></pre>
<br />

Compare `A` and `B` as if they were integers

<a name="compare_as_list-2"></a>

### compare_as_list/2 ###

<pre><code>
compare_as_list(A::term(), B::term()) -&gt; true | false
</code></pre>
<br />

Compare `A` and `B` as if they were lists

<a name="compare_as_string-2"></a>

### compare_as_string/2 ###

<pre><code>
compare_as_string(A::term(), B::term()) -&gt; true | false
</code></pre>
<br />

Compare `A` and `B` as if they were strings

<a name="default_to-2"></a>

### default_to/2 ###

<pre><code>
default_to(Data::term(), Default::term()) -&gt; term()
</code></pre>
<br />

Return `Default` if `Data` is black, `Data` otherwise.

<a name="eval-1"></a>

### eval/1 ###

<pre><code>
eval(Expression::term()) -&gt; {value, term(), list()} | {error, <a href="erl_scan.md#type-error_info">erl_scan:error_info()</a>, <a href="erl_anno.md#type-location">erl_anno:location()</a>} | {error, <a href="erl_parse.md#type-error_info">erl_parse:error_info()</a>}
</code></pre>
<br />

Equivalent to [`eval(Expression, [])`](#eval-2).

<a name="eval-2"></a>

### eval/2 ###

<pre><code>
eval(Expression::term(), Environment::list()) -&gt; {value, term(), list()} | {error, <a href="erl_scan.md#type-error_info">erl_scan:error_info()</a>, <a href="erl_anno.md#type-location">erl_anno:location()</a>} | {error, <a href="erl_parse.md#type-error_info">erl_parse:error_info()</a>}
</code></pre>
<br />

Evaluate the given `Expression` with the given `Environment`.

```

 bucs:eval({toto, 1}).
 % => {value,{toto,1},[]}

 bucs:eval("{toto, 1}").
 % => {value,{toto,1},[]}

 bucs:eval(<<"bucs:eval({toto, 1})">>).
 % => {value,{value,{toto,1},[]},[]}
```

<a name="function_exist-3"></a>

### function_exist/3 ###

`function_exist(Module, Function, Arity) -> any()`

__This function is deprecated:__ use function_exists/3

<a name="function_exists-3"></a>

### function_exists/3 ###

<pre><code>
function_exists(Module::module(), Function::atom(), Arity::integer()) -&gt; true | false
</code></pre>
<br />

Check if the given function exist

<a name="is_kw_list-1"></a>

### is_kw_list/1 ###

<pre><code>
is_kw_list(Data::term()) -&gt; true | false
</code></pre>
<br />

Check if the given value is a keyword list

<a name="is_list_of-2"></a>

### is_list_of/2 ###

<pre><code>
is_list_of(List::list(), Type::<a href="#type-type">type()</a> | atom()) -&gt; true | false
</code></pre>
<br />

Return true if all elements in the `List` are of type `Type`.

```

 bucs:is_list_of([1, 2, 3.4], integer_or_float).
 % => true

 bucs:is_list_of(["hello", "awsome", "world"], string).
 % => true

 bucs:is_list_of(["hello", awsome, "world"], list).
 % => false
```

<a name="is_list_of_lists-1"></a>

### is_list_of_lists/1 ###

<pre><code>
is_list_of_lists(Data::term()) -&gt; true | false
</code></pre>
<br />

Check if the given value is a list of lists

<a name="is_string-1"></a>

### is_string/1 ###

<pre><code>
is_string(Data::term()) -&gt; true | false
</code></pre>
<br />

Check if the given value is a string

<a name="is_tuple_of-2"></a>

### is_tuple_of/2 ###

<pre><code>
is_tuple_of(Tuple::tuple(), Pattern::tuple()) -&gt; true | false
</code></pre>
<br />

Return true if `Tuple` is a tuple matching the tuple `Pattern`

`Pattern` can contains `types()`.

```

 bucs:is_tuple_of({1, "hello", world}, {integer, string, atom}).
 % => true

 bucs:is_tuple_of({1.2, "hello", world}, {integer_or_float, string, atom}).
 % => true

 bucs:is_tuple_of({1, "hello", world}, {integer, string, binary}).
 % => false
```

<a name="is_type-2"></a>

### is_type/2 ###

<pre><code>
is_type(Data::term(), Type::<a href="#type-type">type()</a> | atom()) -&gt; true | false
</code></pre>
<br />

Return true if `Data` is of type `Type`

Type can by any `type()` or a `type()` composition.

```erlang

 is_type("hello", string). % => true
 is_type(1.2, float). % => true
 is_type(3, integer_or_float) % => true

 is_type("hello", integer). % => false
 is_type(1.2, string). % => false
 is_type(hello, string_or_binary_or_list). % => false

 is_type("string", string). % => true
 is_type([1, 2, 3], string). % => false
 is_type("string", list). % => false
 is_type([1, 2, 3], list). % => true
```

<a name="match-2"></a>

### match/2 ###

<pre><code>
match(A::term(), B::term()) -&gt; true | false
</code></pre>
<br />

Return true if `A` match `B`. false otherwise.

<a name="module_exist-1"></a>

### module_exist/1 ###

`module_exist(Module) -> any()`

__This function is deprecated:__ use module_exists/1

<a name="module_exists-1"></a>

### module_exists/1 ###

<pre><code>
module_exists(Module::module()) -&gt; true | false
</code></pre>
<br />

Check if the given module exist

<a name="pipecall-1"></a>

### pipecall/1 ###

<pre><code>
pipecall(Rest::[{function(), [term()]}]) -&gt; term()
</code></pre>
<br />


Pipe fun call

Example:

```

 Add = math:pow(7, 3),
 Log = math:log(Add),
 Mul = multiplication(Log, 7),
 Res = addition(Mul, 7).

 % With bucs:pipecall/1 :
 Res = bucs:pipecall([
                      {fun math:pow/2, [7, 3]},
                      fun math:log/1,
                      {fun multiplication/2, [7]},
                      {fun addition/2, [7]}
                     ]).
```

<a name="present-1"></a>

### present/1 ###

<pre><code>
present(Data::term()) -&gt; true | false
</code></pre>
<br />

Return true if `Data` is _not blank_

<a name="to-2"></a>

### to/2 ###

<pre><code>
to(Type::<a href="#type-type">type()</a>, Data::term()) -&gt; term()
</code></pre>
<br />

Convert `Data` in type `Type`.

<a name="to_atom-1"></a>

### to_atom/1 ###

<pre><code>
to_atom(Data::term()) -&gt; atom()
</code></pre>
<br />


Convert the given term to atom

Example:

```

 atom = bucs:to_atom(atom).
 atom = bucs:to_atom(<<"atom">>).
 atom = bucs:to_atom("atom").
```

<a name="to_binary-1"></a>

### to_binary/1 ###

<pre><code>
to_binary(Data::term()) -&gt; binary()
</code></pre>
<br />


Convert the given term to binary

Example:

```

 <<"list">> = bucs:to_binary(list).
 <<"list">> = bucs:to_binary("list").
 <<"list">> = bucs:to_binary(<<"list">>).
 <<"123">> = bucs:to_binary(123).
 <<"1.20000000000000000000e+01">> = bucs:to_binary(12.0).
 <<"true">> = bucs:to_binary(true).
 <<"false">> = bucs:to_binary(false).
```

<a name="to_float-1"></a>

### to_float/1 ###

<pre><code>
to_float(Data::term()) -&gt; float()
</code></pre>
<br />


Convert the given term to float

Example

```

 123.45 = bucs:to_float(123.45).
 123.45 = bucs:to_float("123.45").
 123.45 = bucs:to_float(<<"123.45">>).
 123.45 = bucs:to_float('123.45').
 123.0 = bucs:to_float(123).
```

<a name="to_float-2"></a>

### to_float/2 ###

<pre><code>
to_float(Data::term(), Precision::integer()) -&gt; float()
</code></pre>
<br />


Convert the given term to float, with the given precision

Example

```

 123.457 = bucs:to_float(123.45678i, 3).
 123.457 = bucs:to_float("123.45678", 3).
 123.457 = bucs:to_float(<<"123.45678">>, 3).
 123.457 = bucs:to_float('123.45678', 3).
 123.0 = bucs:to_float(123, 3).
```

<a name="to_integer-1"></a>

### to_integer/1 ###

<pre><code>
to_integer(Data::term()) -&gt; integer()
</code></pre>
<br />


Convert the given term to integer

Example

```

 123 = bucs:to_integer(123).
 123 = bucs:to_integer("123").
 123 = bucs:to_integer(<<"123">>).
 123 = bucs:to_integer('123').
 123 = bucs:to_integer(123.456).
```

<a name="to_list-1"></a>

### to_list/1 ###

<pre><code>
to_list(Data::term()) -&gt; list()
</code></pre>
<br />


Convert the given term to list

Example:

```

 "list" = bucs:to_list(list).
 "list" = bucs:to_list("list").
 "list" = bucs:to_list(<<"list">>).
 "123" = bucs:to_list(123).
 "1.20000000000000000000e+01" = bucs:to_list(12.0).
 "true" = bucs:to_list(true).
 "false" = bucs:to_list(false).
```

<a name="to_string-1"></a>

### to_string/1 ###

<pre><code>
to_string(Data::term()) -&gt; string()
</code></pre>
<br />

Convert the given term to string

<a name="to_term-1"></a>

### to_term/1 ###

<pre><code>
to_term(Data::term()) -&gt; term()
</code></pre>
<br />


Convert the given value to term

Example

```

 bucs:to_term("{hello, 1}").
 % => {hello, 1}
```

<a name="type-1"></a>

### type/1 ###

<pre><code>
type(Data::term()) -&gt; <a href="#type-type">type()</a>
</code></pre>
<br />

Return the type (atom, string, float, ...) of `Data`

