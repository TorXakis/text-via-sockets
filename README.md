# text-via-sockets
Send and receive `Text` lines using sockets.

This library provides a simple (and opinionated) way to send and receive `Text`
over sockets. It was developed as an alternative to the `listenOn`, `accept`,
and `connectTo` functions in the
[`Network`](https://hackage.haskell.org/package/network-2.6.3.2/docs/Network.html)
library, whose use is advised against in first line of this library's
documentation:

> This module is kept for backwards-compatibility. New users are encouraged to
> use Network.Socket instead.

To accept connections on a given port use `acceptOn`:

```haskell
svrConn <- acceptOn 9090
```
To connect to a host use `connectTo`:

```haskell
cliConn <- connectTo "some-host" 9090
```

Using these connection handles, data can be sent and received using the
`putLineTo` and `getLineFrom` actions:

```haskell
putLineTo svrConn "Hello"
getLineFrom cliConn
putLineTo cliConn "World!"
getLineFrom svrConn
```
Both `acceptOn` and `connectTo` have a retry mechanism (using an exponential
back-off strategy), in case the a connection cannot be (yet) established.

See the [`test`](test/) folder for examples of usage of this library.

## Performance

Here are some benchmark results on Linux, which compare the performance of
this library against using the `hPutLine` and `hGetLine` functions:

```text
enchmarking Text: Lorem Ipsum.
time                 461.7 μs   (444.3 μs .. 488.6 μs)
                     0.960 R²   (0.930 R² .. 0.986 R²)
mean                 483.6 μs   (458.6 μs .. 518.9 μs)
std dev              99.80 μs   (70.83 μs .. 135.3 μs)
variance introduced by outliers: 94% (severely inflated)

benchmarking Text: Lorem Ipsum x100.
time                 21.23 ms   (16.21 ms .. 26.37 ms)
                     0.809 R²   (0.679 R² .. 0.917 R²)
mean                 21.17 ms   (18.93 ms .. 23.33 ms)
std dev              5.243 ms   (3.994 ms .. 6.664 ms)
variance introduced by outliers: 85% (severely inflated)

benchmarking String: Lorem Ipsum.
time                 7.559 ms   (5.459 ms .. 9.498 ms)
                     0.796 R²   (0.755 R² .. 0.964 R²)
mean                 6.271 ms   (5.816 ms .. 6.973 ms)
std dev              1.555 ms   (1.091 ms .. 2.173 ms)
variance introduced by outliers: 91% (severely inflated)

benchmarking String: Lorem Ipsum x100.
time                 393.8 ms   (92.66 ms .. 776.7 ms)
                     0.903 R²   (0.764 R² .. 1.000 R²)
mean                 494.2 ms   (432.2 ms .. 554.1 ms)
std dev              101.9 ms   (0.0 s .. 103.7 ms)
variance introduced by outliers: 48% (moderately inflated)
```

## Debugging

Run `stack` with the following flags:

```sh
stack test --flag "text-via-sockets:debug"
```

## Testing

To run the unit tests simply use:

```sh
stack test
```

This package also includes a simple endurance test that opens and closes a
connection on the same port forever. The goal of this test is to check that no
resources are being leaked. This test is skipped by `stack test`, and it can be
run as follows:

```sh
stack test text-via-sockets:no-resources-leak-test  --ta "simple"
```

Then visit http://localhost:8000/ in your web browser to monitor the resource
usage (using [`ekg`](https://hackage.haskell.org/package/ekg)). The __"Current
residency"__ should not increase over time.
