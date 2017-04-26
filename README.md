# gruff
###### A basic worker pool manager for Erlang to demonstrate the expressive power of gen_pnet.

[![hex.pm](https://img.shields.io/hexpm/v/gruff.svg?style=flat-square)](https://hex.pm/packages/gruff) [![Build Status](https://travis-ci.org/joergen7/gruff.svg?branch=master)](https://travis-ci.org/joergen7/gruff)

This library allows the management of a fixed-size worker pool from which generic worker instances can be allocated, used, and released. Gruff automatically restarts any failing worker and a worker is automatically released if the allocating client fails.

The interface and behavior of the gruff library are intentionally close to the [poolboy](https://github.com/devinus/poolboy) worker pool factory and bears resemblance to other Erlang worker pool managers like [pooler](https://github.com/seth/pooler) or [worker_pool](https://github.com/inaka/worker_pool). This allows the comparison of performance, features, and implementation details. Herein, gruff is the attempt to max out simplicity and clarity in the implementation to showcase the expressive power of the [gen_pnet](https://github.com/joergen7/gen_pnet) behavior.

![gruff Petri net model](priv/gruff_pnet.png)

*Petri net model of the gruff interface and internal behavior with three worker processes.*

## Features

## Usage

### Squaring numbers

### Pooling database connections

## Related worker pool managers

Worker pool managers are a staple of Erlang applications. Here, we compare gruff with several popular Erlang worker pool managers.

### poolboy

- Checkout calls are always blocking (no non-blocking checkout).
- Non-deterministic worker allocation strategy instead of fifo/lifo allocation strategies.
- The number of workers is fixed in gruff (no overflow).
- Gruff prefers `{ok, Result} | {error, Reason}` return values over exception handling.

### pooler

- In gruff (as in poolboy) workers have to implement a worker behavior (`gruff_wrk`) which exposes a `start_link/1` function used to start workers. In pooler, workers are specified via an `{M, F, A}` triple.

### worker_pool

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