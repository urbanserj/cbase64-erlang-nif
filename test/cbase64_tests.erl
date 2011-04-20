-module(cbase64_tests).
-include_lib("eunit/include/eunit.hrl").

get_data() ->
	crypto:rand_bytes(4096).

encode_test() ->
	Data = get_data(),
	A = base64:encode(Data),
	A = cbase64:encode(Data).

decode_test() ->
	Data = base64:encode(get_data()),
	A = base64:encode(Data),
	A = cbase64:encode(Data).

encode_decode_test() ->
	Data = get_data(),
	Data = cbase64:decode( cbase64:encode(Data) ).
