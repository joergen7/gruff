# gruff
###### A basic worker pool manager for Erlang to demonstrate the expressive power of gen_pnet.

[![hex.pm](https://img.shields.io/hexpm/v/gruff.svg?style=flat-square)](https://hex.pm/packages/gruff) [![Build Status](https://travis-ci.org/joergen7/gruff.svg?branch=master)](https://travis-ci.org/joergen7/gruff)

This library allows the management of a fixed-size worker pool from which generic worker instances can be allocated, used, and released. gruff automatically restarts any failing worker and a worker is automatically released if the allocating client fails.

The interface and behavior of the gruff library are intentionally close to the [poolboy](https://github.com/devinus/poolboy) worker pool factory and bears resemblance to other Erlang worker pool managers like [pooler](https://github.com/seth/pooler) or [worker_pool](https://github.com/inaka/worker_pool). This allows the comparison of performance, features, and implementation details. Herein, gruff is the attempt to max out simplicity and clarity in the implementation to showcase the expressive power of the [gen_pnet](https://github.com/joergen7/gen_pnet) behavior.

![gruff Petri net model](priv/gruff_pnet.png)

*Petri net model of the gruff interface and internal behavior with three worker processes.*

## Features

## Usage

### Adding gruff to a Project

#### rebar3

To integrate gruff into a rebar3 managed project change the `deps` entry in your application's `rebar.config` file to include the tuple `{gruff, "0.1.0"}`.

    {deps, [{gruff, "0.1.0"}]}.

#### mix

    {:gruff, "~> 0.1.0"}

### Example: Squaring Numbers

### Example: Pooling Database Connections

## Related Worker Pool Managers

Worker pool managers are a staple of Erlang applications. Here, we compare gruff with several popular Erlang worker pool managers.

### poolboy

- poolboy allows non-blocking checkout which is missing in gruff.
- poolboy uses a fifo/lifo worker allocation strategy while gruff uses a non-deterministic allocation strategy instead.
- Only a part of the worker instances are started right away. Overflow workers are started if this initial worker contingent does not suffice to handle all requests. In contrast, the number of workers in gruff is fixed (no overflow).
- In contrast to gruff, poolboy prefers exception handling over `{ok, Result} | {error, Reason}` return values.

### pooler

- In gruff (as in poolboy) workers have to implement a worker behavior (`gruff_wrk`) which exposes a `start_link/1` function used to start workers. In pooler, workers are started via an `{M, F, A}` triple.
- pooler integrates [exometer](https://github.com/Feuerlabs/exometer) for instrumentation.
- A pooler instance manages several worker pools, while each gruff instance represents only a single pool of homogeneous workers.
- pooler allows pools to be subsumed in groups, thereby adding an additional layer of partitioning.
- pooler allows sending messages to all currently unallocated members of a pool.

### worker_pool

- worker_pool can be configured to issue warnings if workers are kept allocated for too long.
- The workers under worker_pool have to implement either the gen_server or gen_fsm behavior in contrast to poolboy and gruff, where a worker behavior has to be implemented and in contrast to pooler which manages generic worker modules.
- In worker_pool, restarting of failed workers is performed by the supervisor process while poolboy and gruff detect and restart failed workers as a top-level activity.
- Accordingly, the user has complete control over the supervisor process's restart strategy.

## Conclusion

## System Requirements

- Erlang OTP 18.0 or higher
- Rebar3 3.0.0 or higher

## Resources

- [devinus/poolboy](https://github.com/devinus/poolboy). A hunky Erlang worker pool factory.
- [seth/pooler](https://github.com/seth/pooler). An OTP process pool application.
- [inaka/worker_pool](https://github.com/inaka/worker_pool). An Erlang worker pool.
- [aberman/pooly](https://github.com/aberman/pooly). An Erlang OTP process pool.
- [joergen7/gen_pnet](https://github.com/joergen7/gen_pnet). A generic Petri net OTP behavior.

## Authors

- Jorgen Brandt (joergen7) [joergen.brandt@onlinehome.de](mailto:joergen.brandt@onlinehome.de)

## License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)