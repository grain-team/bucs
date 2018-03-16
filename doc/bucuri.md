

# Module bucuri #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-uri_type">uri_type()</a> ###


<pre><code>
uri_type() = http | https | ftp | ssh | sftp
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#join-1">join/1</a></td><td>
Joins a list of URI paths with URI separator.</td></tr><tr><td valign="top"><a href="#join-2">join/2</a></td><td>
Joins two URI paths with URI separator.</td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="join-1"></a>

### join/1 ###

<pre><code>
join(URIs::list()) -&gt; string()
</code></pre>
<br />

Joins a list of URI paths with URI separator.

<a name="join-2"></a>

### join/2 ###

<pre><code>
join(A::string() | binary(), B::string() | binary()) -&gt; string()
</code></pre>
<br />

Joins two URI paths with URI separator.

<a name="type-1"></a>

### type/1 ###

<pre><code>
type(URI::string() | binary()) -&gt; {ok, <a href="#type-uri_type">uri_type()</a>} | error
</code></pre>
<br />

