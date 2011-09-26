%% Copyright (c) 2001-2010 Twisted Matrix Laboratories.
%% Copyright (c) 2011 Peter Lemenkov.
%%
%% The MIT License
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
%%
%% AMP codec.
%%

-module(amp).

-include("../include/amp.hrl").

-export([encode/1]).
-export([decode/1]).
-export([make_tag/0]).

decode(Binary) when is_binary(Binary) ->
	decode(Binary, []).

decode(Binary, DecodedAmps) ->
	try parse_amp_single(Binary, []) of
		{amp, DecodedAmp, <<>>} ->
			{amp, DecodedAmps ++ [DecodedAmp], <<>>};
		{amp, DecodedAmp, Rest} ->
			decode(Rest, DecodedAmps ++ [DecodedAmp])
	catch _:_ ->
		error_logger:warning_msg("bad_amp_raw: ~p~n", [Binary]),
		{amp, DecodedAmps, Binary}
	end.

parse_amp_single(<<0,0, Rest/binary>>, DecodedKVs) ->
	{amp, DecodedKVs, Rest};
parse_amp_single(Data, DecodedKVs) when size(Data) > 1 ->
	{BinKey, Data0} = from_binary(Data),
	{BinVal, Data1} = from_binary(Data0),
	{Key, Val} = case BinKey of
		<<"_command">> ->
			% This key name and command name MUST already exists
			{list_to_existing_atom(binary_to_list(BinKey)), list_to_existing_atom(binary_to_list(BinVal))};
		_ ->
			% This key name MUST already exists
			{list_to_existing_atom(binary_to_list(BinKey)), BinVal}
	end,
	parse_amp_single(Data1, DecodedKVs ++ [{Key, Val}]).

encode(Proplist) when is_list(Proplist) ->
	encode(Proplist, <<>>).

encode([], BinaryAmp) ->
	<<BinaryAmp/binary, 0, 0>>;

encode([{Key, Value} | Rest], BinaryAmp) ->
	BinaryKey = to_binary(Key),
	BinaryVal = to_binary(Value),
	encode(Rest, <<BinaryAmp/binary, BinaryKey/binary, BinaryVal/binary>>).

%%
%% Private functions
%%

make_tag() ->
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	% 4294967296 == INT_MAX
	random:uniform(4294967296).

to_binary(Value) when is_atom (Value) ->
	to_binary(list_to_binary(atom_to_list(Value)));
to_binary(Value) when is_list (Value) ->
	to_binary(list_to_binary(Value));
to_binary(Value) when is_integer (Value) ->
	to_binary(list_to_binary(integer_to_list(Value)));
to_binary(Value) when is_float (Value) ->
	to_binary(list_to_binary(float_to_list(Value)));
to_binary(Value) when is_binary (Value) ->
	Size = size(Value),
	<<Size:16, Value/binary>>.

from_binary(<<Length:16, Data0/binary>> = Data) when size(Data) > 1 ->
	<<Elem:Length/binary, Data1/binary>> = Data0,
	{Elem, Data1}.
