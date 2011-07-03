-module(amp_test).

-behavior(gen_amp_server).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([start_link/2]).
-export(['TestAmpRequest'/1]).

% amp_test:start_link({127,0,0,1}, 1234).
start_link(Ip, Port) ->
        gen_amp_sup:start_link(?MODULE, Ip, Port).

init(_) ->
	{ok, true}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

'TestAmpRequest'(Params, State) when is_list(Params) ->
	error_logger:warning_msg("Params: ~p~n", [Params]),

	% Do something useful

	{noreply_and_close, State}.

%% Tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% TODO

-endif.
