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
%% AMP server base class.
%%

-module(gen_amp_fsm).

-behavior(gen_fsm).

-export([start_link/1]).
-export([init/1]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-export([full/2]).
-export([full/3]).

-record(state, {fd, amp = [], prev = null}).

start_link(Args) ->
	gen_fsm:start_link(?MODULE, Args, []).

init(Fd) ->
	process_flag(trap_exit, true),
	{ok, {Ip, Port}} = inet:peername(Fd),
	error_logger:warning_msg("Socket ~p accepted at ~s:~p by ~p~n", [Fd, inet_parse:ntoa(Ip), Port, self()]),
	{ok, get_key, #state{fd = Fd}}.

full(Event, #state{amp = Amp} = State) ->
	error_logger:warning_msg("Full: ~p when ~p~n", [Event, State]),
	gen_server:cast(gen_amp_server, {amp, Amp, self()}),
	{next_state, get_key, State#state{amp = [], prev = null}}.

full(Event, From, State) ->
	error_logger:warning_msg("Full: ~p from ~p when ~p~n", [Event, From, State]),
	{next_state, get_key, State#state{amp = [], prev = null}}.

handle_event(close, _StateName, State) ->
	{stop, close, State};

handle_event({reply, Amp}, StateName, #state{fd = Fd} = State) ->
	gen_tcp:send(Fd, amp:encode(Amp)),
	{next_state, StateName, State};

handle_event(_Event, StateName, State) ->
	{next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
	{next_state, StateName, State}.

handle_info({tcp, Fd, <<>>}, get_key, #state{fd = Fd, amp = []} = State) ->
	% Bogus empty message (probably keep-alive)
	inet:setopts(Fd, [{active, once}]),
	{next_state, get_key, State};

handle_info({tcp, Fd, <<>>}, get_key, #state{fd = Fd} = State) ->
	inet:setopts(Fd, [{active, once}]),
	% Complete AMP received
	gen_fsm:send_event(self(), full),
	{next_state, full, State};

handle_info({tcp, Fd, B}, get_key, #state{fd = Fd, amp = Amp} = State) ->
	inet:setopts(Fd, [{active, once}]),
	{next_state, get_val, State#state{amp = Amp, prev = list_to_existing_atom(binary_to_list(B))}};

handle_info({tcp, Fd, B}, get_val, #state{fd = Fd, amp = Amp, prev = '_command'} = State) ->
	inet:setopts(Fd, [{active, once}]),
	{next_state, get_key, State#state{amp = Amp ++ [{'_command', list_to_existing_atom(binary_to_list(B))}], prev = null}};

handle_info({tcp, Fd, B}, get_val, #state{fd = Fd, amp = Amp, prev = Key} = State) ->
	inet:setopts(Fd, [{active, once}]),
	{next_state, get_key, State#state{amp = Amp ++ [{Key, B}], prev = null}};

handle_info({tcp_closed, Fd}, StateName, #state{fd = Fd}) ->
	{stop, tcp_closed, StateName}.

terminate(_Reason, _StateName, #state{fd = Fd}) ->
	gen_tcp:close(Fd),
	ok.

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.
