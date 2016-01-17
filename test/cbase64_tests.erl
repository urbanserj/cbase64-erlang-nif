-module(cbase64_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

empty_test() ->
    ?assertEqual(<<>>, cbase64:encode(<<>>)),
    ?assertEqual(<<>>, cbase64:decode(<<>>)).

proper_small_test_() ->
    PropErOpts = [
        {to_file, user},
        {max_size, 64},
        {numtests, 10240}
    ],
    {timeout, 60, ?_assertEqual([], proper:module(?MODULE, PropErOpts))}.

proper_big_test_() ->
    PropErOpts = [
        {to_file, user},
        {max_size, 1048576},
        {numtests, 64}
    ],
    {timeout, 3600, ?_assertEqual([], proper:module(?MODULE, PropErOpts))}.

prop_cbase64_enc_dec() ->
    ?FORALL(Data, binary(),
        begin
            EncData = cbase64:encode(Data),
            EncData = base64:encode(Data),
            Data = cbase64:decode(EncData),
            Data = base64:decode(EncData),
            true
        end).

prop_cbase64_decode() ->
    ?FORALL(Data, binary(),
        begin
            try cbase64:decode(Data) of _ -> true
            catch _:_ -> true
            end
        end).
