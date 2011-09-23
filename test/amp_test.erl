%%
%% Tests
%%

-module(amp_test).

-include_lib("eunit/include/eunit.hrl").

amp_make_cmd_test() ->
	AmpBin = amp:make_cmd("hello", ["foo", "bar"]),
	?assertMatch({amp,[[{'_ask',_}, {'_command',<<"hello">>}, {foo,<<"bar">>}]], <<>>}, amp:decode(AmpBin)).

amp_make_reply_tagInt_test() ->
	AmpBin = amp:make_reply(42, ["foo", "bar"]),
	?assertEqual({amp,[[{'_answer',<<0,0,0,42>>},{foo,<<"bar">>}]], <<>>}, amp:decode(AmpBin)).

amp_make_reply_tagbin_test() ->
	AmpBin = amp:make_reply(<<"tagb">>, ["foo", "bar"]),
	?assertEqual({amp,[[{'_answer',<<"tagb">>},{foo,<<"bar">>}]],<<>>}, amp:decode(AmpBin)).

amp_make_error_tagInt_test() ->
	AmpBin = amp:make_error(42, ["foo", "bar"]),
	?assertEqual({amp,[[{'_error',<<0,0,0,42>>},{foo,<<"bar">>}]],<<>>}, amp:decode(AmpBin)).

amp_make_error_tagbin_test() ->
	AmpBin = amp:make_error(<<"tagb">>, ["foo", "bar"]),
	?assertEqual({amp,[[{'_error',<<"tagb">>},{foo,<<"bar">>}]],<<>>}, amp:decode(AmpBin)).
