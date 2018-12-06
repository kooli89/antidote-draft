-module(test).
-import(s, [init/0,runSend/0, runRecv/0, recvOneMsg/1,consumeLocally/3]).
-include_lib("eunit/include/eunit.hrl").


sendRecv_test() ->
    {NodeId, Socket} = init(),
    MsgList = java:new(NodeId, 'java.util.ArrayList', []),
    Message1 = java:call_static(NodeId,'org.imdea.vcd.Generator', message,["Message 1"]),  
    java:call(MsgList, add, [Message1]),
    SendMsg = consumeLocally(NodeId, Socket, MsgList),
    RcvMsg = recvOneMsg(Socket),
    Test = java:call(SendMsg, equals, [RcvMsg]),
    io:format("Test result: ~p~n", [Test]),
    ?assert(Test).
