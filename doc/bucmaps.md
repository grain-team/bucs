

# Module bucmaps #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#from_list-1">from_list/1</a></td><td>
Takes a list of key-value tuples elements and builds a map ; in deep.</td></tr><tr><td valign="top"><a href="#from_list-2">from_list/2</a></td><td>
Takes a list of key-value tuples elements and builds a map ;
with the given deep.</td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td>
Returns a list of pairs representing the key-value associations of Map ; in deep.</td></tr><tr><td valign="top"><a href="#to_list-2">to_list/2</a></td><td>
Returns a list of pairs representing the key-value associations of Map ;
with the given deep.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="from_list-1"></a>

### from_list/1 ###

<pre><code>
from_list(List::list()) -&gt; map()
</code></pre>
<br />

Takes a list of key-value tuples elements and builds a map ; in deep.

<a name="from_list-2"></a>

### from_list/2 ###

<pre><code>
from_list(List::list(), Deep::integer() | all) -&gt; map()
</code></pre>
<br />

Takes a list of key-value tuples elements and builds a map ;
with the given deep.

<a name="to_list-1"></a>

### to_list/1 ###

<pre><code>
to_list(Map::map()) -&gt; list()
</code></pre>
<br />

Returns a list of pairs representing the key-value associations of Map ; in deep

<a name="to_list-2"></a>

### to_list/2 ###

<pre><code>
to_list(Map::map(), Deep::integer() | all) -&gt; list()
</code></pre>
<br />

Returns a list of pairs representing the key-value associations of Map ;
with the given deep.

