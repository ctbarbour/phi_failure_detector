# Phi Failure Detector

An Erlang implementation of the ["The Phi Accrual Failure Detector" (Hayashibara, et al., 2004)](http://fubica.lsd.ufcg.edu.br/hp/cursos/cfsc/papers/hayashibara04theaccrual.pdf). This implementation is based on the implementation in [Akka](https://github.com/akka/akka/blob/master/akka-remote/src/main/scala/akka/remote/PhiAccrualFailureDetector.scala) and [Cassandra](https://github.com/apache/cassandra/blob/trunk/src/java/org/apache/cassandra/gms/FailureDetector.java).

## Quick Start

Add to a rebar3 project via `rebar.conf`

``` erlang
{deps, [{phi_failure_detector, {git, "https://github.com/ctbarbour/phi_failure_detector.git", {branch, master}}}]}
```

Add to a erlang.mk project via `Makefile`

``` make
DEPS = phi_failure_detector
dep_phi_failure_detector = git https://github.com/ctbarbour/phi_failure_detector.git master
```

To start detecting failures for a service endpoint, start the OTP application and start a new failure detector with a service label and identifier. In this case our service label is `http` and our identifier is `{192,168,10,1}`.

``` erlang
application:ensure_all_started(phi_failure_detector),
phi_failure_detector:new(http, {192,168,10,1})
```

Start adding samples to the failure detector when you get a successful heartbeat from the service endpoint.

``` erlang
phi_failure_detector:heartbeat(http, {192,168,10,1}).
```

Check the φ value of the service.

``` erlang
phi_failure_detector:phi(http, {192,168,10,1}).
```

Get the φ of all endpoints with the same service label.

``` erlang
phi_failure_detector:phi(http).
```

## Description

Phi Accrual Failure Detector is a failure detection algorithm that scales a level of suspicion dynamically based on network conditions over time rather than outputting a binary Up or Down result. For more detailed information I recommend reading the paper, or at least the abstract. To dynamically scale the suspicion level of an endpoint the Phi Accrual Failure Detector records successful heartbeats from a node and builds a distribution of the interarrival times. With this distribution we can calculate the probability that a heartbeat will arrive some time in the future. As network conditions change over time so does the distribution of interarrival times. A node's suspicion is now continuous and not just a binary value. We can make decisions based on how likely it is that a node has failed rather than if thinking in terms of failed or not failed. An application using a Phi Accrual Failure Detector can take precautionary measures when the likelihood of failure has reached a certain threshold and take more drastic measures when the likelihood of failure has reached a higher threshold.

## Build and Test

    $ rebar3 do xref, dialyzer
    $ rebar3 eunit

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/ctbarbour/phi_failure_detector.
