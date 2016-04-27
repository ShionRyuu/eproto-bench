-module(protosun_bench).

-include("simple_pb.hrl").

%% API exports
-export([
    benchmark/1
]).

benchmark(Data) ->
    Bin = simple_pb:encode(Data),
    Data = simple_pb:decode(erlang:element(1, Data), Bin).
