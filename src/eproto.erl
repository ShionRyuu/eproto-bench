-module(eproto).

-include("simple_pb.hrl").

%% API exports
-export([
    benchmark/0,
    benchmark/1
]).

-define(ITERATION, 100000).

%% ====================================================================
%% API functions
%% ====================================================================

benchmark() ->
    benchmark(?ITERATION).

benchmark(Times) ->
    Location = #location{
        region = "Washington", country = "US"
    },
    Person = #person{
        name = "name", phone_number = "202-456-1111",
        address = "The White House 1600 Pennsylvania Avenue, NW Washington, DC 20500",
        age = 27, location = Location
    },
    Methonds = [
        {buildin, fun buildin_bench:benchmark/1}, 
        {gpb, fun gpb_bench:benchmark/1},
        {'protobuf-sunface', fun protosun_bench:benchmark/1}
    ],
    benchmark2(Times, Methonds, Person).

%% ====================================================================
%% Internal functions
%% ====================================================================

benchmark2(Times, Methonds, Data) ->
    [time_action(Times, Tag, Fun, Data) || {Tag, Fun} <- Methonds].

time_action(Times, Tag, Fun, Data) ->
    garbage_collect(),
    T0 = os:timestamp(),
    do_benchmark(Times, Fun, Data),
    T1 = os:timestamp(),
    Elapsed = timer:now_diff(T1, T0) / 1000000,
    io:format("~s ~w iterations, elapsed ~ws~n", [Tag, Times, Elapsed]).

do_benchmark(0, _Fun, _Data) ->
    ok;
do_benchmark(Times, Fun, Data) ->
    Fun(Data),
    do_benchmark(Times - 1, Fun, Data).
