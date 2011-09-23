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

-export([decode/1]).

-export([make_cmd/2]).
-export([make_reply/2]).
-export([make_error/2]).
-export([get_type/1]).
-export([get_command/1]).

decode(Binary) when is_binary(Binary) ->
	decode(Binary, []).

decode(Binary, DecodedAmps) ->
	case parse_amp_single(Binary, []) of
		{amp, DecodedAmp, <<>>} ->
			{amp, DecodedAmps ++ [DecodedAmp], <<>>};
		{amp, DecodedAmp, Rest} ->
			decode(Rest, DecodedAmps ++ [DecodedAmp]);
		error ->
			{amp, DecodedAmps, Binary}
	end.

parse_amp_single(<<0,0, Rest/binary>>, DecodedKVs) ->
	{amp, DecodedKVs, Rest};

parse_amp_single(Data, DecodedKVs) when size(Data) > 1 ->
	try
		{Key, Data0} = from_binary(Data),
		{Val, Data1} = from_binary(Data0),
		parse_amp_single(Data1, DecodedKVs ++ [{list_to_atom(binary_to_list(Key)), Val}])
	catch _:_ ->
		error_logger:error_msg("bad_amp_raw: ~p~n", [Data]),
		error
	end;

parse_amp_single(Rest, _DecodedKVs) ->
	error_logger:warning_msg("bad_amp_raw: ~p~n", [Rest]),
	error.

get_type([{?ASK, _AmpTag} , {?COMMAND, _CmdName} | _RestPropList]) ->
	?ASK;
get_type([{?ERROR, _AmpTag} , {?COMMAND, _CmdName} | _RestPropList]) ->
	?ERROR;
get_type([{?ANSWER, _AmpTag} , {?COMMAND, _CmdName} | _RestPropList]) ->
	?ANSWER.

get_command([{?ASK, AmpTag} , {?COMMAND, CmdName} | RestPropList]) ->
	% This command name MUST already exists
	{AmpTag, list_to_existing_atom(binary_to_list(CmdName)), RestPropList}.

make_reply(Tag, Args) when is_list (Args), is_integer(Tag) ->
	make_reply(<<Tag:32>>, Args);
make_reply(Tag, Args) when is_list (Args), is_binary(Tag) ->
	TagSize = size(Tag),
	make_amp_raw(Args, <<7:16, "_answer", TagSize:16, Tag/binary>>).
make_error(Tag, Args) when is_list (Args), is_integer(Tag) ->
	make_error(<<Tag:32>>, Args);
make_error(Tag, Args) when is_list (Args), is_binary(Tag) ->
	TagSize = size(Tag),
	make_amp_raw(Args, <<6:16, "_error", TagSize:16, Tag/binary>>).
make_cmd(Command, Args) when is_list (Args) ->
	AmpTag = make_tag(),
	BinCommand = to_binary(Command),
	make_amp_raw(Args, <<4:16, "_ask", 4:16, AmpTag:32, 8:16, "_command", BinCommand/binary>>).

make_amp_raw([], BinaryAmp) when is_binary(BinaryAmp) ->
	<<BinaryAmp/binary, 0, 0>>;

make_amp_raw([{Key, Value} | Rest], BinaryAmp) when is_binary(BinaryAmp) ->
	make_amp_raw([Key, Value | Rest], BinaryAmp);
make_amp_raw([Key, Value | Rest], BinaryAmp) when is_binary(BinaryAmp) ->
	BinaryKey = to_binary(Key),
	BinaryVal = to_binary(Value),
	make_amp_raw(Rest, <<BinaryAmp/binary, BinaryKey/binary, BinaryVal/binary>>).

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
