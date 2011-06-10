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

-export([start/3]).
-export([start_link/3]).

-export([accept_loop/2]).
-export([read_loop/2]).

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
	modstate,
	sock,
	apid,
	clients = []
}).

%%%%%%%%%%%%%%%%%%

start(Module, Ip, Port) ->
        gen_server:start(?MODULE, [Module, Ip, Port], []).

start_link(Module, Ip, Port) ->
        gen_server:start_link(?MODULE, [Module, Ip, Port], []).

%%%%%%%%%%%%%%%%%%

init([Module, Ip, Port]) ->
	process_flag(trap_exit, true),
	{ok, ListenSocket} = gen_tcp:listen(Port, [{ip, Ip}, binary, {reuseaddr, true}, {active, false}]),
	AcceptorPid = spawn_link(node(), ?MODULE, accept_loop, [self(), ListenSocket]),
	{ok, ModState} = Module:init(hello),
	{ok, #state{

			mod=Module,
			modstate=ModState,
			sock=ListenSocket,
			apid = AcceptorPid
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

handle_cast({accepted, Socket}, State = #state{clients=Clients}) ->
	spawn_link(node(), ?MODULE, read_loop, [self(), Socket]),
	{noreply, State#state{clients = Clients ++ [{Socket, <<>>}]}};

handle_cast({read, Socket, B}, State = #state{mod=Module, clients=Clients}) ->
	PrevBytes = proplists:get_value(Socket, Clients),
	{amp, DecodedKVs, Rest} = amp:parse_amp(<<PrevBytes/binary, B/binary>>),
	lists:foreach(fun (X) -> process_amp(Module, X, Socket) end, DecodedKVs),
	{noreply, State#state{clients = proplists:delete(Socket, Clients) ++ [{Socket, Rest}]}};

handle_cast({closed, Socket}, State = #state{clients=Clients}) ->
	{noreply, State#state{clients = proplists:delete(Socket, Clients)}};

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


terminate(Reason, State = #state{mod=Mod, modstate=ModState, sock=ListenSocket}) ->
	error_logger:warning_msg("Terminated ~p due to ~p~n", [self(), Reason]),
%	Mod:terminate(Reason, ModState),
	ok = gen_tcp:close(ListenSocket),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accept_loop(Server, ListenSocket) ->
	case gen_tcp:accept(ListenSocket) of
		{ok, Socket} ->
			{ok, {Ip, Port}} = inet:peername(Socket),
			error_logger:warning_msg("Socket ~p accepted at ~s:~p by ~p~n", [Socket, inet_parse:ntoa(Ip), Port, self()]),
			gen_server:cast(Server, {accepted, Socket}),
			accept_loop(Server, ListenSocket);
		{error, Error} ->
			error_logger:warning_msg("ListenSocket ~p ~p~n", [ListenSocket, Error])
	end.

read_loop(Server, Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, B} ->
			gen_server:cast(Server, {read, Socket, B}),
			read_loop(Server, Socket);
		{error, Error} ->
			error_logger:warning_msg("Socket ~p ~p~n", [Socket, Error]),
			gen_server:cast(Server, {closed, Socket})
	end.

process_amp(Mod, Amp, Socket) ->
	case amp:get_type(Amp) of
		?ASK ->
			{Tag, Cmd, Opts} = amp:get_command(Amp),
			try Mod:Cmd(proplists:delete(eof, Opts)) of
				{reply, noreply} ->
					error_logger:warning_msg("Got answer for ~p but noreply was thrown~n", [Amp]),
					ok;
				{reply, Reply} ->
					gen_tcp:send(Socket, amp:make_reply(Tag, Reply));
				{error, Error} ->
					error_logger:error_msg("Got error error:~p for ~p~n", [Error, Amp]),
					gen_tcp:send(Socket, amp:make_error(Tag, Error))
			catch
				ExceptionClass:ExceptionPattern ->
					error_logger:error_msg("Got exception ~p:~p for ~p~n", [ExceptionClass, ExceptionPattern, Amp]),
					gen_tcp:send(Socket, amp:make_error(Tag, [{exception, list_to_binary(atom_to_list(ExceptionClass))}]))
			end;
		?ERROR ->
			ok;
		?ANSWER ->
			ok
	end.

