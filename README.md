# text-via-sockets
Send and receive `Text` lines using sockets.

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

This package also includes a simple endurance test, that opens and closes a
connection on the same port forever. The goal of this test is to check that no
resources are being leaked. This test is skipped by `stack test`, and it can be
run as follows:

```sh
stack test text-via-sockets:no-resources-leak-test  --ta "simple"
```

Then visit http://localhost:8000/ in your web browser to monitor the resource
usage (using [`ekg`](https://hackage.haskell.org/package/ekg)). The __"Current
residency"__ should not increase over time.
