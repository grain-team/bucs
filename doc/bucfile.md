

# Module bucfile #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-copy_option">copy_option()</a> ###


<pre><code>
copy_option() = recursive | {exclude, [<a href="file.md#type-filename">file:filename()</a>]} | {only, [<a href="file.md#type-filename">file:filename()</a>]} | <a href="#type-copyfile_option">copyfile_option()</a>
</code></pre>




### <a name="type-copyfile_option">copyfile_option()</a> ###


<pre><code>
copyfile_option() = preserve_file_info | default_file_info | {directory_mode, integer()} | {regular_file_mode, integer()} | {executable_file_mode, integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#copy-2">copy/2</a></td><td>Equivalent to <a href="#copy-3"><tt>copy(Source, Destination,
[preserve_file_info, recursive])</tt></a>.</td></tr><tr><td valign="top"><a href="#copy-3">copy/3</a></td><td>
Copy a <tt>Source</tt> to a <tt>Destination</tt></td></tr><tr><td valign="top"><a href="#copyfile-2">copyfile/2</a></td><td>Equivalent to <a href="#copyfile-3"><tt>copyfile(Source, Destination, [preserve_file_info])</tt></a>.</td></tr><tr><td valign="top"><a href="#copyfile-3">copyfile/3</a></td><td>
Copy file <tt>Source</tt> to a <tt>Destination</tt></td></tr><tr><td valign="top"><a href="#expand_path-1">expand_path/1</a></td><td>
Expand the given path.</td></tr><tr><td valign="top"><a href="#is_broken-1">is_broken/1</a></td><td>
Return true if <tt>Path</tt> is a broken symlink.</td></tr><tr><td valign="top"><a href="#is_executable-1">is_executable/1</a></td><td>
Return true if <tt>File</tt> is executable, false otherwise.</td></tr><tr><td valign="top"><a href="#is_executable-2">is_executable/2</a></td><td>
Return true if <tt>File</tt> is executable for <tt>Who</tt>, false otherwise.</td></tr><tr><td valign="top"><a href="#is_symlink-1">is_symlink/1</a></td><td>
Return true if <tt>Path</tt> is a symlink, false otherwise.</td></tr><tr><td valign="top"><a href="#make_dir-1">make_dir/1</a></td><td>
Create the given directory if it not exist.</td></tr><tr><td valign="top"><a href="#match-2">match/2</a></td><td>Equivalent to <a href="#match-3"><tt>match(Path, Exp, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#match-3">match/3</a></td><td>
Return true if the <tt>Path</tt> match the <tt>Expression</tt></td></tr><tr><td valign="top"><a href="#normalize_path-1">normalize_path/1</a></td><td>
Normalize the given path.</td></tr><tr><td valign="top"><a href="#realpath-1">realpath/1</a></td><td>
Return the realpath of the given path.</td></tr><tr><td valign="top"><a href="#relative_from-2">relative_from/2</a></td><td>
Return the given <tt>FilePath</tt> relatively to the <tt>FromPath</tt>.</td></tr><tr><td valign="top"><a href="#remove_recursive-1">remove_recursive/1</a></td><td>
Remove, recursively the given path.</td></tr><tr><td valign="top"><a href="#user_home-0">user_home/0</a></td><td>
Return the HOME directory.</td></tr><tr><td valign="top"><a href="#wildcard-2">wildcard/2</a></td><td>Equivalent to <a href="#wildcard-3"><tt>wildcard(Path, Exclude, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#wildcard-3">wildcard/3</a></td><td>
Same as <tt>filelib:wildcard/1</tt> but where expressions listed in <tt>Exclude</tt> are excluded.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="copy-2"></a>

### copy/2 ###

<pre><code>
copy(Source::<a href="file.md#type-filename_all">file:filename_all()</a> | <a href="file.md#type-dirname_all">file:dirname_all()</a>, Destination::<a href="file.md#type-filename_all">file:filename_all()</a> | <a href="file.md#type-dirname_all">file:dirname_all()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Equivalent to [`copy(Source, Destination,[preserve_file_info, recursive])`](#copy-3).

<a name="copy-3"></a>

### copy/3 ###

<pre><code>
copy(Source::<a href="file.md#type-filename_all">file:filename_all()</a> | <a href="file.md#type-dirname_all">file:dirname_all()</a>, Destination::<a href="file.md#type-filename_all">file:filename_all()</a> | <a href="file.md#type-dirname_all">file:dirname_all()</a>, Options::[<a href="#type-copy_option">copy_option()</a>]) -&gt; ok | {error, term()}
</code></pre>
<br />

Copy a `Source` to a `Destination`

Available options:

* `recursive`

* `{exclude, [file:filename()]}`

* `{only, [file:filename()]}`

* `preserve_file_info` (default)

* `default_file_info`

* `{directory_mode, integer()}`

* `{regular_file_mode, integer()}`

* `{executable_file_mode, integer()}`


<a name="copyfile-2"></a>

### copyfile/2 ###

<pre><code>
copyfile(Source::<a href="file.md#type-filename_all">file:filename_all()</a>, Destination::<a href="file.md#type-filename_all">file:filename_all()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Equivalent to [`copyfile(Source, Destination, [preserve_file_info])`](#copyfile-3).

<a name="copyfile-3"></a>

### copyfile/3 ###

<pre><code>
copyfile(Source::<a href="file.md#type-filename_all">file:filename_all()</a>, Destination::<a href="file.md#type-filename_all">file:filename_all()</a>, Options::[<a href="#type-copyfile_option">copyfile_option()</a>]) -&gt; ok | {error, term()}
</code></pre>
<br />

Copy file `Source` to a `Destination`

Available options:

* `preserve_file_info` (default)

* `default_file_info`

* `{directory_mode, integer()}`

* `{regular_file_mode, integer()}`

* `{executable_file_mode, integer()}`


<a name="expand_path-1"></a>

### expand_path/1 ###

<pre><code>
expand_path(Path::string() | binary()) -&gt; binary() | list()
</code></pre>
<br />

Expand the given path

Example:

```erlang

  "/home/user" = bucfile:expand_path("~").
  <<"/home/user">> = bucfile:expand_path(<<"~">>).
```

<a name="is_broken-1"></a>

### is_broken/1 ###

`is_broken(Path) -> any()`

Return true if `Path` is a broken symlink.

<a name="is_executable-1"></a>

### is_executable/1 ###

<pre><code>
is_executable(File::<a href="file.md#type-name_all">file:name_all()</a>) -&gt; map()
</code></pre>
<br />

Return true if `File` is executable, false otherwise

<a name="is_executable-2"></a>

### is_executable/2 ###

<pre><code>
is_executable(File::<a href="file.md#type-filename">file:filename()</a>, Who::owner | group | other | list()) -&gt; true | false
</code></pre>
<br />

Return true if `File` is executable for `Who`, false otherwise

<a name="is_symlink-1"></a>

### is_symlink/1 ###

`is_symlink(Path) -> any()`

Return true if `Path` is a symlink, false otherwise

<a name="make_dir-1"></a>

### make_dir/1 ###

<pre><code>
make_dir(Path::string() | binary()) -&gt; ok | {error, term()}
</code></pre>
<br />

Create the given directory if it not exist

<a name="match-2"></a>

### match/2 ###

`match(Path, Exp) -> any()`

Equivalent to [`match(Path, Exp, [])`](#match-3).

<a name="match-3"></a>

### match/3 ###

`match(Path, Expression, Options) -> any()`

Return true if the `Path` match the `Expression`

The exclude string looks like a wildcard string.

`Options:`

* `expand_path` : the `Path` wil be expanded using `bucfile:expand_path/1`

* `{cd, From}` : the `Path` will be prefixed with `From`


Example:

```

  bucfile:match("a/b/c", "**/b/**").
  % => true
  bucfile:match("a/b/c", "**/a/**").
  % => false
  bucfile:match("/a/b/c", "**/a/**").
  % => true
  bucfile:match("a/b/c", "**/a/**", [expand_path]).
  % => true
  bucfile:match("a/b/c", "**/a/**", [{cd, "/tmp"}]).
  % => true
  bucfile:match("a/b/c", "**/tmp/**", [{cd, "/tmp"}]).
  % => true
  bucfile:match("a/b/c", "**/tmp/**", [{cd, "tmp"}]).
  % => false
  bucfile:match("a/b/c", "**/tmp/**", [expand_path, {cd, "tmp"}]).
  % => true
```

<a name="normalize_path-1"></a>

### normalize_path/1 ###

<pre><code>
normalize_path(Path::string() | binary()) -&gt; string() | binary()
</code></pre>
<br />

Normalize the given path

Example:

```erlang

  "/" = bucfile:normalize_path("/toto/tutu/../../../../../..").
  <<"/">> = bucfile:normalize_path(<<"/toto/tutu/../../../../../..">>).
  "/toto/titi" = bucfile:normalize_path("/toto/tata/../titi").
```

<a name="realpath-1"></a>

### realpath/1 ###

<pre><code>
realpath(Path::<a href="file.md#type-dirname_all">file:dirname_all()</a>) -&gt; <a href="file.md#type-dirname_all">file:dirname_all()</a>
</code></pre>
<br />

Return the realpath of the given path

<a name="relative_from-2"></a>

### relative_from/2 ###

<pre><code>
relative_from(FilePath::<a href="file.md#type-filename_all">file:filename_all()</a>, FromPath::<a href="file.md#type-dirname_all">file:dirname_all()</a>) -&gt; <a href="file.md#type-filename_all">file:filename_all()</a> | error
</code></pre>
<br />

Return the given `FilePath` relatively to the `FromPath`.

<a name="remove_recursive-1"></a>

### remove_recursive/1 ###

<pre><code>
remove_recursive(Path::string() | binary()) -&gt; ok | {error, term()}
</code></pre>
<br />

Remove, recursively the given path

<a name="user_home-0"></a>

### user_home/0 ###

<pre><code>
user_home() -&gt; string()
</code></pre>
<br />

Return the HOME directory

Example:

```erlang

  "/home/user" = bucfile:user_home().
```

<a name="wildcard-2"></a>

### wildcard/2 ###

`wildcard(Path, Exclude) -> any()`

Equivalent to [`wildcard(Path, Exclude, [])`](#wildcard-3).

<a name="wildcard-3"></a>

### wildcard/3 ###

`wildcard(Path, Exclude, Options) -> any()`

Same as `filelib:wildcard/1` but where expressions listed in `Exclude` are excluded.

The exclude string looks like a wildcard string.

`Options:`

* `expand_path` : the `Path` wil be expanded using `bucfile:expand_path/1`

* `{cd, From}` : the `Path` will be prefixed with `From`


