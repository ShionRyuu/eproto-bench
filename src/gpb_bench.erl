-module(gpb_bench).

-include("simple.hrl").

%% API exports
-export([
    benchmark/1
]).

benchmark(Data) ->
    Bin = simple:encode_msg(Data),
    Data = simple:decode_msg(Bin, erlang:element(1, Data)).
