eproto-sunface-bench
=====

benchmark for erlang proto

Build
-----

```sh
$ ./protoc-erl proto src include

=INFO REPORT==== 27-Apr-2016::21:41:34 ===
Writing header file to "include/simple_pb.hrl"

=INFO REPORT==== 27-Apr-2016::21:41:34 ===
Writing src file to "src/simple_pb.erl"

=INFO REPORT==== 27-Apr-2016::21:41:34 ===
Writing header file to "include/protobuffs.hrl"
$ rebar3 compile
===> Verifying dependencies...
===> Fetching protoc-erl2 ({git,
                                      "https://github.com/ShionRyuu/protoc-erl2",
                                      {branch,"master"}})
===> Compiling protoc-erl
===> Compiling eproto
```

Benchmark
-----

```sh
$ ./proto-bench
buildin 100000 iterations, elapsed 0.25729s
gpb 100000 iterations, elapsed 0.874354s
protobuf-sunface 100000 iterations, elapsed 1.450913s
```
