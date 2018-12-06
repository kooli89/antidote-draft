antidote draft
======

### Dependencies
- [JavaErlang binding library](https://github.com/fredlund/JavaErlang)
- Jar file of the [VCD-java-client](https://github.com/vitorenesduarte/VCD-java-client) Master branch

### Usage
1. Run the `localTesting.sh` script in terminal 1
```
$ ./localTesting.sh
```
2. Make sure the Jar file of the VCD-java-client named `vcd.jar` exists in the current directory
3. In terminal 2, run an infinite receiving loop
```
$ erl -sname loopRcv
> c(connector).
> connector:runRecv().
```
4. In terminal 3, send messages
```
$ erl -sname loopSend
> c(connector).
> connector:runSend().
```
### Tests
```
$ erlc connector.erl test.erl
$ erl -sname test -noshell -pa ebin -eval "test:test()" -s init stop
```