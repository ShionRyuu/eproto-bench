-module(buildin_bench).

-include("simple.hrl").

%% API exports
-export([
    benchmark/1
]).

benchmark(Data) ->
    Bin = erlang:term_to_binary(Data),
    Data = erlang:binary_to_term(Bin).
