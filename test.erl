-module(test).
-import(connector, [init/0,recvOneMsg/1,send/3]).
-include_lib("eunit/include/eunit.hrl").


sendRecv_test() ->
    {NodeId, Socket} = init(),
    MsgList = java:new(NodeId, 'java.util.ArrayList', []),
    Message1 = java:call_static(NodeId,'org.imdea.vcd.Generator', message,["Message 1"]),  
    java:call(MsgList, add, [Message1]),
    SendMsg = send(NodeId, Socket, MsgList),
    {RcvMsg, Status} = recvOneMsg(Socket),
    Test = java:call(SendMsg, equals, [RcvMsg]),
    ?assert(Test).

status_test() ->
    {NodeId, Socket} = init(),
    MsgList = java:new(NodeId, 'java.util.ArrayList', []),
    Message1 = java:call_static(NodeId,'org.imdea.vcd.Generator', message,["Message 1"]),  
    java:call(MsgList, add, [Message1]),
    SendMsg = send(NodeId, Socket, MsgList),
    {RcvMsg, Status} = recvOneMsg(Socket),
    io:format("received status ~p~n",[Status]),
    ?assert(Status == 2).
