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
	BaseDir = case code:priv_dir(?MODULE) of
		{error, bad_name} ->
			filename:join( [ filename:dirname( code:which(?MODULE) ), "..", "priv" ] );
		Dir ->
			Dir
		end,
	SoName = filename:join(BaseDir, atom_to_list(?MODULE)),
	erlang:load_nif(SoName, 0).

encode(block) ->
	base64:encode(block).

decode(block) ->
	base64:decode(block).
