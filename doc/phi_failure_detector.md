

# Module phi_failure_detector #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#heartbeat-2">heartbeat/2</a></td><td>Add a successful heartbeat to the sample set for calculating Phi.</td></tr><tr><td valign="top"><a href="#monitor-3">monitor/3</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates a new Phi Failure Detector.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Creates a new Phi Failure Detector.</td></tr><tr><td valign="top"><a href="#phi-1">phi/1</a></td><td>Calculate Phi for all endpoints that share the same <code>Label</code>.</td></tr><tr><td valign="top"><a href="#phi-2">phi/2</a></td><td>Calculate the Phi value for a service endpoint.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="heartbeat-2"></a>

### heartbeat/2 ###

<pre><code>
heartbeat(Label, ID) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Label = atom()</code></li><li><code>ID = term()</code></li></ul>

Add a successful heartbeat to the sample set for calculating Phi

When a heartbeat arrives we store the inter-arrival time from the previously received heartbeat
and store it in a fix sized sliding window. The data regarding the oldest heartbeat is dropped
from window.

<a name="monitor-3"></a>

### monitor/3 ###

<pre><code>
monitor(Label, ID, Threshold) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Label = atom()</code></li><li><code>ID = term()</code></li><li><code>Threshold = float()</code></li></ul>

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Label, ID) -&gt; {ok, Pid}
</code></pre>

<ul class="definitions"><li><code>Label = atom()</code></li><li><code>ID = term()</code></li><li><code>Pid = pid()</code></li></ul>

Creates a new Phi Failure Detector

[`new/3`](#new-3)

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Label, ID, Opts) -&gt; {ok, Pid}
</code></pre>

<ul class="definitions"><li><code>Label = atom()</code></li><li><code>ID = term()</code></li><li><code>Opts = list()</code></li><li><code>Pid = pid()</code></li></ul>

Creates a new Phi Failure Detector

Creating a new Phi Failure Detector creates a process to manage the sampling window of
inter-arrival times of heartbeats and allows a client to query the Phi value at a point in time.
The unique identifier for a Phi Failure Detector is the combination of `Label` and `ID`.  The
`Label` and `ID` combination are used with [`heartbeat/2`](#heartbeat-2).

<a name="phi-1"></a>

### phi/1 ###

<pre><code>
phi(Label) -&gt; Status
</code></pre>

<ul class="definitions"><li><code>Label = atom()</code></li><li><code>Status = [{{atom(), term()}, float()}]</code></li></ul>

Calculate Phi for all endpoints that share the same `Label`.

See [`phi/2`](#phi-2) for more details on Phi.

<a name="phi-2"></a>

### phi/2 ###

<pre><code>
phi(Label, ID) -&gt; Phi
</code></pre>

<ul class="definitions"><li><code>Label = atom()</code></li><li><code>ID = term()</code></li><li><code>Phi = float()</code></li></ul>

Calculate the Phi value for a service endpoint

Phi represents a dynamically scalable suspicion level of a service endpoint based on the
inter-arrival times recorded with each [`heartbeat/2`](#heartbeat-2). A distribution of
inter-arrival times are used to compute the value of Phi at some point in time. The estimation of
inter-arrival times assumes a normal distribution. The follow function is used to determine phi:
-log10(1 - cdf(tnow - tlast))

