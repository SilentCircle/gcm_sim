

# Module gcm_sim #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

gcm_sim startup code.

Copyright (c) 2013 Silent Circle, LLC.

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Start the gcm_sim server.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>Start the gcm_sim server.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Starts the app for inclusion in a supervisor tree.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>Starts the app for inclusion in a supervisor tree.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>Stop the gcm_sim server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start-0"></a>

### start/0 ###

<pre><code>
start() -&gt; ok
</code></pre>
<br />

Start the gcm_sim server.

<a name="start-1"></a>

### start/1 ###

<pre><code>
start(Env) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Env = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li></ul>

Start the gcm_sim server.

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, Pid::pid()}
</code></pre>
<br />

Starts the app for inclusion in a supervisor tree

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Env::list()) -&gt; {ok, Pid::pid()}
</code></pre>
<br />

Starts the app for inclusion in a supervisor tree.

<a name="stop-0"></a>

### stop/0 ###

<pre><code>
stop() -&gt; ok
</code></pre>
<br />

Stop the gcm_sim server.

