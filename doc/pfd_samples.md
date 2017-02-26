

# Module pfd_samples #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-samples">samples()</a> ###


<pre><code>
samples() = #pfd{samples = [float()], max_sample_size = pos_integer(), min_std_dev = float(), last_sample = undefined | pos_integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-1">add/1</a></td><td></td></tr><tr><td valign="top"><a href="#add-2">add/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr><tr><td valign="top"><a href="#phi-1">phi/1</a></td><td></td></tr><tr><td valign="top"><a href="#phi-2">phi/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-1"></a>

### add/1 ###

<pre><code>
add(PhiAccrualFailureDetector0) -&gt; PhiAccrualFailureDetector
</code></pre>

<ul class="definitions"><li><code>PhiAccrualFailureDetector0 = <a href="#type-samples">samples()</a></code></li><li><code>PhiAccrualFailureDetector = <a href="#type-samples">samples()</a></code></li></ul>

<a name="add-2"></a>

### add/2 ###

<pre><code>
add(Timestamp, PhiAccrualFailureDetector0) -&gt; PhiAccrualFailureDetector
</code></pre>

<ul class="definitions"><li><code>Timestamp = pos_integer()</code></li><li><code>PhiAccrualFailureDetector0 = <a href="#type-samples">samples()</a></code></li><li><code>PhiAccrualFailureDetector = <a href="#type-samples">samples()</a></code></li></ul>

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; PhiAccrualFailureDetector
</code></pre>

<ul class="definitions"><li><code>PhiAccrualFailureDetector = <a href="#type-samples">samples()</a></code></li></ul>

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(MaxSampleSize, MinStdDev, FirstHeartbeatEstimate) -&gt; PhiAccrualFailureDetector
</code></pre>

<ul class="definitions"><li><code>MaxSampleSize = pos_integer()</code></li><li><code>MinStdDev = float()</code></li><li><code>FirstHeartbeatEstimate = float()</code></li><li><code>PhiAccrualFailureDetector = <a href="#type-samples">samples()</a></code></li></ul>

<a name="phi-1"></a>

### phi/1 ###

<pre><code>
phi(PhiAccrualFailureDetector) -&gt; Phi
</code></pre>

<ul class="definitions"><li><code>PhiAccrualFailureDetector = <a href="#type-samples">samples()</a></code></li><li><code>Phi = float()</code></li></ul>

<a name="phi-2"></a>

### phi/2 ###

<pre><code>
phi(Timestamp, PhiAccrualFailureDetector) -&gt; Phi
</code></pre>

<ul class="definitions"><li><code>Timestamp = pos_integer()</code></li><li><code>PhiAccrualFailureDetector = <a href="#type-samples">samples()</a></code></li><li><code>Phi = float() | infinity</code></li></ul>

