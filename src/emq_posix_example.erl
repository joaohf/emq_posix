-module(emq_posix_example).
-compile(export_all).

-include("emq_posix.hrl").

-define(QUEUETEST, "/Teste7").

%%
%% Simple 
%%

simple_send_receive_select() ->
	application:start(emq_posix),
	
	Qw = createq(emq_posix:create(?QUEUETEST, 0, 777, 5, 1000, write)),
	Qr = createq(emq_posix:open(?QUEUETEST, 0, read)),	
	
	Pid2 = spawn(?MODULE, send_mq, [Qw]),	
	Pid1 = spawn(?MODULE, recv_mq, [Qr]),
	
	emq_posix:select(Qr),
	
	Pid2 ! {send, "Teste Teste Teste 1"},
	Pid2 ! {send, "Teste Teste Teste 2"},
	Pid2 ! {send, "Teste Teste Teste 3"},
	
%% 	Pid1 ! {rcv_mq},
%% 	Pid1 ! {rcv_mq},
%% 	Pid1 ! {rcv_mq},
	
%%	recv_mq_select(),
	
	emq_posix:close(Qw),
    emq_posix:close(Qr),    
    emq_posix:remove(?QUEUETEST),
    application:stop(emq_posix).

simple_send_receive() ->
	application:start(emq_posix),
	
	Qw = createq(emq_posix:create(?QUEUETEST, 0, 777, 5, 1000, write)),
	Qr = createq(emq_posix:open(?QUEUETEST, 0, read)),	
	
	Pid2 = spawn(?MODULE, send_mq, [Qw]),	
	Pid1 = spawn(?MODULE, recv_mq, [Qr]),    
	
	Pid2 ! {send, "Teste Teste Teste 1"},
	Pid2 ! {send, "Teste Teste Teste 2"},
	Pid2 ! {send, "Teste Teste Teste 3"},
	
	Pid1 ! {rcv_mq},
	Pid1 ! {rcv_mq},
	Pid1 ! {rcv_mq},
	
	
	timer:sleep(15000),
	
	emq_posix:close(Qw),
    emq_posix:close(Qr),    
    emq_posix:remove(?QUEUETEST),
    application:stop(emq_posix).

recv_mq_select() ->	
	receive
		{data, _, _, Data} -> 
					io:format("FD: ~w ~n", [Data]),
					recv_mq_select()
	end.

recv_mq(Queue) ->	
	receive
		{rcv_mq} -> Res = emq_posix:recv(Queue),
					io:format("FD: ~w ~n", [Res]),
					{_, _, _, Data} = Res,
					MyString = binary_to_term(Data),
					io:format("FD Term: ~w ~n", [MyString]);
		_ -> recv_mq(Queue)
	end,
	recv_mq(Queue).

send_mq(Queue) ->	
	receive
		{send, Msg} -> emq_posix:send(Queue, 1, Msg);
		_ -> {error, unknow_msg}
	end,
	send_mq(Queue).

simplesend() ->
     application:start(emq_posix),     
     Qw = createq(emq_posix:create(?QUEUETEST, 0, 777, 5, 1000, write)),
	 
     timer:sleep(2000),
     Qr = createq(emq_posix:open(?QUEUETEST, 0, read)),
	 
     timer:sleep(2000),
     io:format("FD: ~w ~w ~n", [Qw, Qr]),
     io:get_chars("next step>", 1),
     emq_posix:send(Qw, 1, "Teste Teste Teste"),
     timer:sleep(2000),
     emq_posix:close(Qw),
     emq_posix:close(Qr),
     emq_posix:remove(?QUEUETEST),
     emq_posix:remove(?QUEUETEST),
     application:stop(emq_posix).

createq({mqd, Desc}) -> 
	Desc;
createq({error, Err}) ->
	{error, Err}.

error_report({error, What}) ->
	io:format("ERROR: ~w ~n", [What]).
