-module(test).
-import(vcd_connector, [init/0, recv_one_msg/1, send/3]).
-include_lib("eunit/include/eunit.hrl").

%% @doc This function tests the sending and receiving mechanism
send_recv_test() ->
    {NodeId, Socket} = init(),
    MsgList = java:new(NodeId, 'java.util.ArrayList', []),
    Message1 = java:call_static(NodeId,'org.imdea.vcd.Generator', message,["Message 1"]),  
    java:call(MsgList, add, [Message1]),
    SendMsg = send(NodeId, Socket, MsgList),
    {RcvMsg, _} = recv_one_msg(Socket),
    Test = java:call(SendMsg, equals, [RcvMsg]),
    ?assert(Test).


%% @doc This function tests the message status reception
status_test() ->
    {NodeId, Socket} = init(),
    MsgList = java:new(NodeId, 'java.util.ArrayList', []),
    Message1 = java:call_static(NodeId,'org.imdea.vcd.Generator', message,["Message 1"]),  
    java:call(MsgList, add, [Message1]),
    _ = send(NodeId, Socket, MsgList),
    {_, Status} = recv_one_msg(Socket),
    ?assert(Status == 2).
