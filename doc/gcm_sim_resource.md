

# Module gcm_sim_resource #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

GCM Simulator webmachine_resource.

Copyright (c) 2013 Silent Circle, LLC.

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

<a name="types"></a>

## Data Types ##




### <a name="type-config">config()</a> ###


<pre><code>
config() = <a href="#type-pl">pl</a>(atom(), term())
</code></pre>




### <a name="type-ctx">ctx()</a> ###


<pre><code>
ctx() = #ctx{}
</code></pre>




### <a name="type-debug_info">debug_info()</a> ###


<pre><code>
debug_info() = ok | {trace, <a href="#type-filepath">filepath()</a>}
</code></pre>




### <a name="type-filepath">filepath()</a> ###


<pre><code>
filepath() = string()
</code></pre>




### <a name="type-pl">pl()</a> ###


<pre><code>
pl(KT, VT) = [{KT, VT}]
</code></pre>




### <a name="type-wbool_ret">wbool_ret()</a> ###


<pre><code>
wbool_ret() = <a href="#type-wret">wret</a>(boolean())
</code></pre>




### <a name="type-werr">werr()</a> ###


<pre><code>
werr() = {error, iolist()}
</code></pre>




### <a name="type-wiolist_ret">wiolist_ret()</a> ###


<pre><code>
wiolist_ret() = <a href="#type-wret">wret</a>(iolist())
</code></pre>




### <a name="type-wret">wret()</a> ###


<pre><code>
wret(T) = {T | <a href="#type-werr">werr()</a>, <a href="#type-wrq">wrq()</a>, <a href="#type-ctx">ctx()</a>}
</code></pre>




### <a name="type-wrq">wrq()</a> ###


<pre><code>
wrq() = #wm_reqdata{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#allowed_methods-2">allowed_methods/2</a></td><td></td></tr><tr><td valign="top"><a href="#content_types_accepted-2">content_types_accepted/2</a></td><td></td></tr><tr><td valign="top"><a href="#content_types_provided-2">content_types_provided/2</a></td><td></td></tr><tr><td valign="top"><a href="#finish_request-2">finish_request/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#malformed_request-2">malformed_request/2</a></td><td></td></tr><tr><td valign="top"><a href="#post_is_create-2">post_is_create/2</a></td><td></td></tr><tr><td valign="top"><a href="#process_post-2">process_post/2</a></td><td></td></tr><tr><td valign="top"><a href="#simulate_gcm_resp-2">simulate_gcm_resp/2</a></td><td></td></tr><tr><td valign="top"><a href="#to.md-2">to_html/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_text-2">to_text/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="allowed_methods-2"></a>

### allowed_methods/2 ###

<pre><code>
allowed_methods(ReqData::<a href="#type-wrq">wrq()</a>, Context::<a href="#type-ctx">ctx()</a>) -&gt; {list(), <a href="#type-wrq">wrq()</a>, <a href="#type-ctx">ctx()</a>}
</code></pre>
<br />

<a name="content_types_accepted-2"></a>

### content_types_accepted/2 ###

<pre><code>
content_types_accepted(ReqData::<a href="#type-wrq">wrq()</a>, Context::<a href="#type-ctx">ctx()</a>) -&gt; {list(), <a href="#type-wrq">wrq()</a>, <a href="#type-ctx">ctx()</a>}
</code></pre>
<br />

<a name="content_types_provided-2"></a>

### content_types_provided/2 ###

<pre><code>
content_types_provided(ReqData::<a href="#type-wrq">wrq()</a>, Context::<a href="#type-ctx">ctx()</a>) -&gt; {list(), <a href="#type-wrq">wrq()</a>, <a href="#type-ctx">ctx()</a>}
</code></pre>
<br />

<a name="finish_request-2"></a>

### finish_request/2 ###

<pre><code>
finish_request(ReqData::<a href="#type-wrq">wrq()</a>, Context::<a href="#type-ctx">ctx()</a>) -&gt; <a href="#type-wbool_ret">wbool_ret()</a>
</code></pre>
<br />

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(Config::<a href="#type-config">config()</a>) -&gt; {<a href="#type-debug_info">debug_info()</a>, <a href="#type-ctx">ctx()</a>}
</code></pre>
<br />

<a name="malformed_request-2"></a>

### malformed_request/2 ###

<pre><code>
malformed_request(ReqData::<a href="#type-wrq">wrq()</a>, Ctx::<a href="#type-ctx">ctx()</a>) -&gt; <a href="#type-wbool_ret">wbool_ret()</a>
</code></pre>
<br />

<a name="post_is_create-2"></a>

### post_is_create/2 ###

<pre><code>
post_is_create(ReqData::<a href="#type-wrq">wrq()</a>, Context::<a href="#type-ctx">ctx()</a>) -&gt; <a href="#type-wbool_ret">wbool_ret()</a>
</code></pre>
<br />

<a name="process_post-2"></a>

### process_post/2 ###

<pre><code>
process_post(ReqData::<a href="#type-wrq">wrq()</a>, Context::<a href="#type-ctx">ctx()</a>) -&gt; <a href="#type-wbool_ret">wbool_ret()</a>
</code></pre>
<br />

<a name="simulate_gcm_resp-2"></a>

### simulate_gcm_resp/2 ###

<pre><code>
simulate_gcm_resp(ReqData::<a href="#type-wrq">wrq()</a>, Context::<a href="#type-ctx">ctx()</a>) -&gt; <a href="#type-wbool_ret">wbool_ret()</a>
</code></pre>
<br />

<a name="to_html-2"></a>

### to_html/2 ###

<pre><code>
to_html(ReqData::<a href="#type-wrq">wrq()</a>, Context::<a href="#type-ctx">ctx()</a>) -&gt; <a href="#type-wiolist_ret">wiolist_ret()</a>
</code></pre>
<br />

<a name="to_text-2"></a>

### to_text/2 ###

<pre><code>
to_text(ReqData::<a href="#type-wrq">wrq()</a>, Context::<a href="#type-ctx">ctx()</a>) -&gt; <a href="#type-wiolist_ret">wiolist_ret()</a>
</code></pre>
<br />

