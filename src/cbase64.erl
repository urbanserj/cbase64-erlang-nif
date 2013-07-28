%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% Copyright (c) 2011 Sergey Urbanovich
%% http://github.com/urbanserj/cbase64-erlang-nif
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(cbase64).
-export([encode/1, decode/1]).

-on_load(on_load/0).

on_load() ->
    BaseDir =
        case code:priv_dir(?MODULE) of
            {error, bad_name} ->
                EbinDir = filename:dirname(code:which(?MODULE)),
                AppPath = filename:dirname(EbinDir),
                filename:join(AppPath, "priv");
            Dir ->
                Dir
        end,
    SoName = filename:join(BaseDir, atom_to_list(?MODULE)),
    erlang:load_nif(SoName, 0).


-spec encode(binary() | iolist()) -> binary().
encode(Data) ->
    encode(Data, undefined, 0).

-spec encode(binary() | iolist(), binary() | undefined,
        non_neg_integer()) -> binary().
encode(Data, Buf, BufSize) ->
    case nif_encode(Data, Buf, BufSize) of
        {Data0, Buf0, BufSize0} ->
            encode(Data0, Buf0, BufSize0);
        Buf0 ->
            Buf0
    end.


-spec decode(binary() | iolist()) -> binary().
decode(Data) ->
    decode(Data, undefined, 0).

-spec decode(binary() | iolist(), binary() | undefined,
        non_neg_integer()) -> binary().
decode(Data, Buf, BufSize) ->
    case nif_decode(Data, Buf, BufSize) of
        {Data0, Buf0, BufSize0} ->
            decode(Data0, Buf0, BufSize0);
        Buf0 ->
            Buf0
    end.

-spec nif_encode(binary() | iolist(), binary() | undefined, non_neg_integer()) ->
        binary() | {binary(), binary(), non_neg_integer()}.
nif_encode(_Data, _Buf, _Size) ->
    erlang:nif_error(not_loaded, [{module, ?MODULE}, {line, ?LINE}]).

-spec nif_decode(binary() | iolist(), binary() | undefined, non_neg_integer()) ->
        binary() | {binary(), binary(), non_neg_integer()}.
nif_decode(_Data, _Buf, _Size) ->
    erlang:nif_error(not_loaded, [{module, ?MODULE}, {line, ?LINE}]).
