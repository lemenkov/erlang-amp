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

-module(gen_amp_server).

-behavior(gen_server).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([behaviour_info/1]).

-export([start_link/1]).

-include("../include/amp.hrl").

%% @private
behaviour_info(callbacks) ->
	[
		{init, 1},
		{handle_call, 3},
		{handle_cast, 2},
		{handle_info, 2},
		{terminate, 2},
		{code_change, 3}
	];
behaviour_info(_) ->
	undefined.

%%%%%%%%%%%%%%%%%%

-record(state, {
	mod,
	modstate
}).

%%%%%%%%%%%%%%%%%%

start_link(Module) ->
        gen_server:start_link({local, ?MODULE}, ?MODULE, [Module], []).

%%%%%%%%%%%%%%%%%%

init([Module]) ->
	process_flag(trap_exit, true),
	{ok, ModState} = Module:init(hello),
	{ok, #state{

			mod=Module,
			modstate=ModState
		}
	}.

handle_call(Request, From, State = #state{mod=Module, modstate=ModState}) ->
	case Module:handle_call(Request, From, ModState) of
		{reply, Reply, NewModState} ->
			{reply, Reply, State#state{modstate=NewModState}};
		{noreply, NewModState} ->
			{noreply, State#state{modstate=NewModState}};
		{stop, Reason, NewModState} ->
			{stop, Reason, State#state{modstate=NewModState}};
		{stop, Reason, Reply, NewModState} ->
			{stop, Reason, Reply, State#state{modstate=NewModState}}
	 end.

handle_cast({amp, [{?ASK, Tag}, {?COMMAND, Cmd} | Opts] = Amp, Client}, #state{mod=Module, modstate=ModState} = State) when is_atom(Cmd) ->
	try Module:Cmd(Opts, ModState) of
		{noreply, NewState}->
			error_logger:warning_msg("Got answer for ~p but noreply was thrown~n", [Amp]),
			{noreply, State#state{modstate=NewState}};
		{noreply_and_close, NewState} ->
			error_logger:warning_msg("Got answer for ~p but noreply_and_close was thrown~n", [Amp]),
			gen_fsm:send_all_state_event(Client, close),
			{noreply, State#state{modstate=NewState}};
		{reply, Reply, NewState} ->
			gen_fsm:send_all_state_event(Client, {reply, [{'_answer', Tag}] ++ Reply}),
			{noreply, State#state{modstate=NewState}};
		{reply_and_close, Reply, NewState} ->
			gen_fsm:send_all_state_event(Client, {reply, [{'_answer', Tag}] ++ Reply}),
			gen_fsm:send_all_state_event(Client, close),
			{noreply, State#state{modstate=NewState}};
		{error, Error, NewState} ->
			error_logger:error_msg("Got error:~p for ~p~n", [Error, Amp]),
			gen_fsm:send_all_state_event(Client, {reply, [{'_error', Tag}] ++ Error}),
			{noreply, State#state{modstate=NewState}}
	catch
		ExceptionClass:ExceptionPattern ->
			error_logger:error_msg("Got exception ~p:~p for ~p~n", [ExceptionClass, ExceptionPattern, Amp]),
			gen_fsm:send_all_state_event(Client, {reply, [{'_error', Tag}] ++ [{ExceptionClass, ExceptionPattern}]}),
			% TODO should we close socket here?
			{noreply, State}
	end;

handle_cast({amp, [{?ERROR, _Tag} | _Opts] = _Amp, _Client}, #state{mod = _Module, modstate = _ModState} = State) ->
	% TODO what should we do in case then client returnes an error message?
	{noreply, State};

handle_cast({amp, [{?ANSWER, _Tag} | _Opts] = _Amp, _Client}, #state{mod = _Module, modstate = _ModState} = State) ->
	% TODO what should we do in case then client answers?
	{noreply, State};

handle_cast(Request, State = #state{mod=Module, modstate=ModState}) ->
	case Module:handle_cast(Request, ModState) of
		{noreply, NewModState} ->
			{noreply, State#state{modstate=NewModState}};
		{stop, Reason, NewModState} ->
			{stop, Reason, State#state{modstate=NewModState}}
	end.

handle_info(Info, State = #state{mod=Module, modstate=ModState}) ->
	case Module:handle_info(Info, ModState) of
		{noreply, NewModState} ->
			{noreply, State#state{modstate=NewModState}};
		{stop, Reason, NewModState} ->
			{stop, Reason, State#state{modstate=NewModState}}
	end.

terminate(Reason, _State = #state{mod=Mod, modstate=ModState}) ->
	error_logger:warning_msg("Terminated ~p due to ~p~n", [self(), Reason]),
	Mod:terminate(Reason, ModState).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
