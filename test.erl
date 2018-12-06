-module(test).
-import(s, [init/0,runSend/0, runRecv/0, recvOneMsg/1]).
-include_lib("eunit/include/eunit.hrl").

sendRecvTest() ->
    {NodeId, Socket} = init(),
    MsgList = java:new(NodeId, 'java.util.ArrayList', []),
    Message1 = java:call_static(NodeId,'org.imdea.vcd.Generator', message,["Message 1"]),  
    java:call(MsgList, add, [Message1]),
    consumeLocally(NodeId, Socket, MsgList),
    RcvBytes = recvOneMsg(Socket),
