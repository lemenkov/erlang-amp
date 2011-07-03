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

-export([make_cmd/2]).
-export([make_reply/2]).
-export([make_error/2]).
-export([parse_amp/1]).
-export([get_type/1]).
-export([get_command/1]).

parse_amp(Binary) when is_binary(Binary) ->
	parse_amp(Binary, []).

parse_amp(Binary, DecodedAmps) ->
	case parse_amp_single(Binary, []) of
		{amp, DecodedAmp, <<>>} ->
			{amp, DecodedAmps ++ [DecodedAmp], <<>>};
		{amp, DecodedAmp, Rest} ->
			parse_amp(Rest, DecodedAmps ++ [DecodedAmp]);
		error ->
			{amp, DecodedAmps, Binary}
	end.

parse_amp_single(<<0,0, Rest/binary>>, DecodedKVs) ->
	{amp, DecodedKVs ++ [{eof}], Rest};

parse_amp_single(<<LengthK:16, Data/binary>> = Data0, DecodedKVs) ->
	try
		<<Key:LengthK/binary, LengthV:16, R1/binary>> = Data,
		<<Val:LengthV/binary, R2/binary>> = R1,
		parse_amp_single(R2, DecodedKVs ++ [{list_to_atom(binary_to_list(Key)), Val}])
	catch _:_ ->
		error_logger:error_msg("bad_amp_raw: ~p~n", [Data0]),
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
	{SizeCommand, BinCommand} = to_binary(Command),
	make_amp_raw(Args, <<4:16, "_ask", 4:16, AmpTag:32, 8:16, "_command", SizeCommand:16, BinCommand/binary>>).

make_amp_raw([], BinaryAmp) when is_binary(BinaryAmp) ->
	<<BinaryAmp/binary, 0, 0>>;

make_amp_raw([{Key, Value} | Rest], BinaryAmp) when is_binary(BinaryAmp) ->
	make_amp_raw([Key, Value | Rest], BinaryAmp);
make_amp_raw([Key, Value | Rest], BinaryAmp) when is_binary(BinaryAmp) ->
	{BinaryKeySize, BinaryKey} = to_binary(Key),
	{BinaryValueSize, BinaryValue} = to_binary(Value),
	make_amp_raw(Rest, <<BinaryAmp/binary, BinaryKeySize:16, BinaryKey/binary, BinaryValueSize:16, BinaryValue/binary>>).

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
	{size(Value), Value}.

%% Tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%amp_test() ->
%	AmpBin = amp:make_amp("hello", ["foo", "bar"]),
%	AmpParsed = amp:parse_amp(AmpBin),
%	?assertEqual(AmpParsed, amp:parse_amp(AmpBin)).

-endif.
