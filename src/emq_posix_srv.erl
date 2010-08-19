%% Copyright (c) 2010, Mazen Harake
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%     * Redistributions of source code must retain the above copyright notice,
%%       this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

-module(emq_posix_srv).
-behaviour(gen_server).
-include("emq_posix.hrl").
-include("emq_posix_commands.hrl").

%% Behaviour Callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3]).

%% Module API
-export([start_link/0, call/2, getch/0]).

%% Records
-record(state, { port, qdesc, observer }).

%% =============================================================================
%% Module API
%% =============================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

call(Cmd, Args) ->
    gen_server:call(?MODULE, {call, Cmd, Args}, infinity).

getch() ->
    gen_server:call(?MODULE, getch, infinity).

%% =============================================================================
%% Behaviour Callbacks
%% =============================================================================
init(no_args) ->
    process_flag(trap_exit, true),
    case load_driver() of
	ok ->
	    Port = erlang:open_port({spawn, "emq_posix"}, [binary]),
	    {ok, #state{ port = Port }};
	{error, ErrorCode} ->
	    exit({driver_error, erl_ddll:format_error(ErrorCode)})
    end.

handle_call({call, Cmd, Args}, _From, State) ->
    {reply, do_call(State#state.port, Cmd, Args), State};
handle_call(getch, From, #state{ qdesc = undefined } = State) ->
    {noreply, State#state{ qdesc = From }};
handle_call(getch, _From, State) ->
    {reply, -1, State}.

terminate(_Reason, State) ->
    do_call(State#state.port, ?CLOSE_QUEUE, State#state.qdesc),
	do_call(State#state.port, ?REMOVE_QUEUE, State#state.qdesc),
    erlang:port_close(State#state.port),
    erl_ddll:unload("emq_posix").

handle_info({_Port, {data, _Binary}}, #state{ qdesc = undefined } = State) ->
    {noreply, State};
handle_info({_Port, {data, Binary}}, State) ->
    gen_server:reply(State#state.qdesc, binary_to_term(Binary)),
    {noreply, State#state{ qdesc = undefined }}.

%% @hidden
handle_cast(_, State) ->
    {noreply, State}.

%% @hidden
code_change(_, State, _) ->
    {noreply, State}.

%% =============================================================================
%% Internal Functions
%% =============================================================================
do_call(Port, Cmd) ->
    do_call(Port, Cmd, undefined).

do_call(Port, Cmd, Args) ->
    binary_to_term(erlang:port_control(Port, Cmd, term_to_binary(Args))).

load_driver() ->
    Dir = case code:priv_dir(cecho) of
              {error, bad_name} ->
                  filename:dirname(code:which(?MODULE)) ++ "/../priv";
              D ->
                  D
          end,
    erl_ddll:load(Dir, "emq_posix").
