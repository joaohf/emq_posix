%% Copyright (c) 2010, João Henrique Ferreira de Freitas
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

-module(emq_posix).
-behaviour(application).
-include("include/emq_posix.hrl").
-include("include/emq_posix_commands.hrl").

%% Behaviour Callbacks
-export([start/2, stop/1]).

%% Application API

-export([open/3, create/6, close/1, remove/1, getattr/1,
         send/3, recv/1, select/2, deselect/1]). 

%% =============================================================================
%% Application API
%% =============================================================================
open(QueueName, Blocking, read) when is_list(QueueName) andalso is_integer(Blocking) ->
	call(?OPEN_QUEUE_RDONLY, {QueueName, Blocking});
open(QueueName, Blocking, write) when is_list(QueueName) andalso is_integer(Blocking) ->
	call(?OPEN_QUEUE_WDONLY, {QueueName, Blocking});
open(QueueName, Blocking, readwrite) when is_list(QueueName) andalso is_integer(Blocking) ->
	call(?OPEN_QUEUE_RDWR, {QueueName, Blocking}).

create(QueueName, Blocking, Mode, Maxmsg, Msgsize, read) when is_list(QueueName) andalso is_integer(Blocking)
                                                        andalso is_integer(Mode) andalso is_integer(Maxmsg)
                                                        andalso is_integer(Msgsize) ->
        call(?CREATE_QUEUE_RDONLY, {QueueName, Blocking, Mode, Maxmsg, Msgsize});
create(QueueName, Blocking, Mode, Maxmsg, Msgsize, write) when is_list(QueueName) andalso is_integer(Blocking)
                                                        andalso is_integer(Mode) andalso is_integer(Maxmsg)
                                                        andalso is_integer(Msgsize) ->
        call(?CREATE_QUEUE_WDONLY, {QueueName, Blocking, Mode, Maxmsg, Msgsize});
create(QueueName, Blocking, Mode, Maxmsg, Msgsize, readwrite) when is_list(QueueName) andalso is_integer(Blocking)
                                                        andalso is_integer(Mode) andalso is_integer(Maxmsg)
                                                        andalso is_integer(Msgsize) ->
        call(?CREATE_QUEUE_RDWR, {QueueName, Blocking, Mode, Maxmsg, Msgsize}).

close(QueueDesc) when is_integer(QueueDesc) ->
        call(?CLOSE_QUEUE, QueueDesc).

remove(QueueName) when is_list(QueueName) ->
        call(?REMOVE_QUEUE, QueueName).

getattr(QueueDesc) when is_integer(QueueDesc) ->
        call(?GETATTR_QUEUE, QueueDesc).

send(QueueDesc, Priority, Msg) when is_integer(QueueDesc) andalso is_integer(Priority) ->
        _Bin = term_to_binary(Msg),
        _Size = size(_Bin),
        call(?SEND_QUEUE, {QueueDesc, Priority, _Size, _Bin}).

recv(QueueDesc) when is_integer(QueueDesc) ->
        call(?RECEIVE_QUEUE, QueueDesc).

select(QueueDesc, DestPid) when is_integer(QueueDesc) andalso is_pid(DestPid)->
        call(?SELECT_QUEUE, {QueueDesc, DestPid}).

deselect(QueueDesc) when is_integer(QueueDesc) ->
        call(?DESELECT_QUEUE, QueueDesc).

%% =============================================================================
%% Behaviour Callbacks
%% =============================================================================
start(_, _) ->
    emq_posix_srv:start_link().

stop(_) ->
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================
call(Cmd) ->
    call(Cmd, undefined).

call(Cmd, Args) ->
    emq_posix_srv:call(Cmd, Args).
