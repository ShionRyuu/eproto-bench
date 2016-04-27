-file("src/simple_pb.erl", 1).

-module(simple_pb).

-export([encode/1, encode/2, decode/2,
	 encode_location/1, decode_location/1, encode_person/1,
	 decode_person/1]).

-record(location, {region, country}).

-record(person,
	{name, address, phone_number, age, location}).

encode(Record) ->
    encode(erlang:element(1, Record), Record).

encode_location(Record)
    when is_record(Record, location) ->
    encode(location, Record).

encode_person(Record) when is_record(Record, person) ->
    encode(person, Record).

encode(person, _Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(_Record#person.name, none), string, []),
		      pack(2, required,
			   with_default(_Record#person.address, none), string,
			   []),
		      pack(3, required,
			   with_default(_Record#person.phone_number, none),
			   string, []),
		      pack(4, required,
			   with_default(_Record#person.age, none), int32, []),
		      pack(5, optional,
			   with_default(_Record#person.location, none),
			   location, [])]);
encode(location, _Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(_Record#location.region, none), string,
			   []),
		      pack(2, required,
			   with_default(_Record#location.country, none), string,
			   [])]).

with_default(undefined, none) -> undefined;
with_default(undefined, Default) -> Default;
with_default(Val, _) -> Val.

pack(_, optional, undefined, _, _) -> [];
pack(_, repeated, undefined, _, _) -> [];
pack(FNum, required, undefined, Type, _) ->
    exit({error,
	  {required_field_is_undefined, FNum, Type}});
pack(_, repeated, [], _, Acc) -> lists:reverse(Acc);
pack(FNum, repeated, [Head | Tail], Type, Acc) ->
    pack(FNum, repeated, Tail, Type,
	 [pack(FNum, optional, Head, Type, []) | Acc]);
pack(FNum, _, Data, _, _) when is_tuple(Data) ->
    RecName = erlang:element(1, Data),
    protobuffs:encode(FNum, encode(RecName, Data), bytes);
pack(FNum, _, Data, Type, _) ->
    protobuffs:encode(FNum, Data, Type).

decode_location(Bytes) -> decode(location, Bytes).

decode_person(Bytes) -> decode(person, Bytes).

decode(person, Bytes) ->
    Types = [{5, location, location, [is_record]},
	     {4, age, int32, []}, {3, phone_number, string, []},
	     {2, address, string, []}, {1, name, string, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(person, Decoded);
decode(location, Bytes) ->
    Types = [{2, country, string, []},
	     {1, region, string, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(location, Decoded).

decode(<<>>, _, Acc) -> Acc;
decode(<<Bytes/binary>>, Types, Acc) ->
    {{FNum, WireType}, Rest} =
	protobuffs:read_field_num_and_wire_type(Bytes),
    case lists:keysearch(FNum, 1, Types) of
      {value, {FNum, Name, Type, Opts}} ->
	  {Value1, Rest1} = case lists:member(is_record, Opts) of
			      true ->
				  {V, R} = protobuffs:decode_value(WireType,
								   bytes, Rest),
				  RecVal = decode(Type, V),
				  {RecVal, R};
			      false ->
				  {V, R} = protobuffs:decode_value(WireType,
								   Type, Rest),
				  {unpack_value(V, Type), R}
			    end,
	  case lists:member(repeated, Opts) of
	    true ->
		case lists:keytake(FNum, 1, Acc) of
		  {value, {FNum, Name, List}, Acc1} ->
		      decode(Rest1, Types,
			     [{FNum, Name,
			       lists:reverse([Value1 | lists:reverse(List)])}
			      | Acc1]);
		  false ->
		      decode(Rest1, Types, [{FNum, Name, [Value1]} | Acc])
		end;
	    false ->
		decode(Rest1, Types, [{FNum, Name, Value1} | Acc])
	  end;
      false -> exit({error, {unexpected_field_index, FNum}})
    end.

unpack_value(<<Binary/binary>>, string) ->
    binary_to_list(Binary);
unpack_value(Value, _) -> Value.

to_record(person, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, person), Record,
					 Name, Val)
		end,
		#person{}, DecodedTuples);
to_record(location, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, location), Record,
					 Name, Val)
		end,
		#location{}, DecodedTuples).

set_record_field(Fields, Record, Field, Value) ->
    Index = list_index(Field, Fields),
    erlang:setelement(Index + 1, Record, Value).

list_index(Target, List) -> list_index(Target, List, 1).

list_index(Target, [Target | _], Index) -> Index;
list_index(Target, [_ | Tail], Index) ->
    list_index(Target, Tail, Index + 1);
list_index(_, [], _) -> 0.

