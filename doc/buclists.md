

# Module buclists #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete_if-2">delete_if/2</a></td><td>
Remove alla elements from the <tt>List</tt> when <tt>Fun(Element)</tt> return false.</td></tr><tr><td valign="top"><a href="#keyfind-3">keyfind/3</a></td><td>Equivalent to <a href="#keyfind-4"><tt>keyfind(Key, N, TupleList, false)</tt></a>.</td></tr><tr><td valign="top"><a href="#keyfind-4">keyfind/4</a></td><td>
Searches the list of tuples <tt>TupleList</tt> for a tuple whose
<tt>N</tt>th element compares equal to <tt>Key</tt>.</td></tr><tr><td valign="top"><a href="#keyfind-5">keyfind/5</a></td><td>
Searches the list of tuples <tt>TupleList</tt> for a tuple whose
<tt>N</tt>th element compares equal to <tt>Key</tt>.</td></tr><tr><td valign="top"><a href="#keyufind-3">keyufind/3</a></td><td>Equivalent to <a href="#keyufind-4"><tt>keyufind(Key, N, TupleList, false)</tt></a>.</td></tr><tr><td valign="top"><a href="#keyufind-4">keyufind/4</a></td><td></td></tr><tr><td valign="top"><a href="#keyupdate-4">keyupdate/4</a></td><td>
Add or replace the <tt>Tuple</tt> in the <tt>List</tt>.</td></tr><tr><td valign="top"><a href="#merge_keylists-3">merge_keylists/3</a></td><td>
Merge the two keylists.</td></tr><tr><td valign="top"><a href="#nsplit-2">nsplit/2</a></td><td> 
Split the given list in N lists.</td></tr><tr><td valign="top"><a href="#pipemap-2">pipemap/2</a></td><td>
Run all <tt>Funs</tt> on each elements of the <tt>List</tt></td></tr><tr><td valign="top"><a href="#splitn-2">splitn/2</a></td><td> 
Split the given list in multiples lists of maximum N elements.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete_if-2"></a>

### delete_if/2 ###

<pre><code>
delete_if(Fun::fun((term()) -&gt; true | false), List::list()) -&gt; list()
</code></pre>
<br />

Remove alla elements from the `List` when `Fun(Element)` return false.

<a name="keyfind-3"></a>

### keyfind/3 ###

<pre><code>
keyfind(Key::term(), N::integer(), TupleList::[tuple()]) -&gt; term()
</code></pre>
<br />

Equivalent to [`keyfind(Key, N, TupleList, false)`](#keyfind-4).

<a name="keyfind-4"></a>

### keyfind/4 ###

<pre><code>
keyfind(Key::term(), N::integer(), TupleList::[tuple()], Default::term()) -&gt; term()
</code></pre>
<br />

Searches the list of tuples `TupleList` for a tuple whose
`N`th element compares equal to `Key`.
Returns the second element of the Tuple if such a tuple is found
and has only two elements, the tuple if it has more than two elements,
otherwise `Default`.

<a name="keyfind-5"></a>

### keyfind/5 ###

<pre><code>
keyfind(Key::term(), N::integer(), TupleList::[tuple()], M::integer(), Default::term()) -&gt; term()
</code></pre>
<br />

Searches the list of tuples `TupleList` for a tuple whose
`N`th element compares equal to `Key`.
Returns the `M`th element of the Tuple if such a tuple is found,
otherwise `Default`.

<a name="keyufind-3"></a>

### keyufind/3 ###

<pre><code>
keyufind(Key::term(), N::integer(), TupleList::[tuple()]) -&gt; term()
</code></pre>
<br />

Equivalent to [`keyufind(Key, N, TupleList, false)`](#keyufind-4).

<a name="keyufind-4"></a>

### keyufind/4 ###

<pre><code>
keyufind(Key::term(), N::integer(), TupleList::[tuple()], Default::term()) -&gt; term()
</code></pre>
<br />

<a name="keyupdate-4"></a>

### keyupdate/4 ###

<pre><code>
keyupdate(Key::term(), N::integer(), List::list(), Tuple::tuple()) -&gt; list()
</code></pre>
<br />

Add or replace the `Tuple` in the `List`.

<a name="merge_keylists-3"></a>

### merge_keylists/3 ###

`merge_keylists(N, Rest, TupleList2) -> any()`

Merge the two keylists.

Example:

```

  Args = [{a, 1}, {b, 2}],
  Default = [{b, 3}, {c, 4}],
  elists:merge_keylists(1, Args, Default),
    #=> [{c, 4}, {a, 1}, {b, 2}]
```

<a name="nsplit-2"></a>

### nsplit/2 ###

`nsplit(List, N) -> any()`


Split the given list in N lists

Example:

```

 buclists:nsplit([a,b,c,d,e,f,g], 3).
   #=> [[a,b],[c,d],[e,f,g]]
```

<a name="pipemap-2"></a>

### pipemap/2 ###

<pre><code>
pipemap(Funs::[fun((term()) -&gt; term())], List::list()) -&gt; list()
</code></pre>
<br />

Run all `Funs` on each elements of the `List`

<a name="splitn-2"></a>

### splitn/2 ###

`splitn(List, N) -> any()`


Split the given list in multiples lists of maximum N elements.

Example:

```

 buclists:splitn([a,b,c,d,e,f,g], 3).
   #=> [[a,b,c],[d,e,f],[g]]
```

