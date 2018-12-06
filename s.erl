-module(s).
-export([init/0, runSend/0, runRecv/0, recvOneMsg/1]).

init() ->
    application:ensure_started(java_erlang),
    java:set_timeout(infinity),
    {ok,NodeId} = java:start_node([{add_to_java_classpath,["vcd.jar"]}]),
    Conf = java:call_static(NodeId,'org.imdea.vcd.Config',parseArgs,[""]),
    Socket = java:call_static(NodeId,'org.imdea.vcd.Socket',create,[Conf,10]),
    {NodeId, Socket}.


recvOneMsg(Socket) ->
    RcvSet = java:call(Socket,'receive',[]),
    RcvBytes = java:call(java:call(java:call(java:call(RcvSet,getMessagesList,[]),get,[0]),getData,[]),toByteArray,[]),
    io:format("received ~p~n",[java:array_to_list(RcvBytes)]),
    Status = java:call(RcvSet, getStatusValue, []),
    io:format("received status ~p~n",[Status]),
    RcvBytes.


recv(Socket) ->
    RcvBytes = recvOneMsg(Socket),
    io:format("received ~p~n",[java:array_to_list(RcvBytes)]),
    timer:sleep(1000),
    recv(Socket).


consumeLocally(NodeId, Socket, MsgList) ->
    Array = java:call(MsgList, toArray, []),
    io:format("Message List: ~p~n", [java:array_to_list(Array)]),

    MgbMsgSetBuilder = java:call_static(NodeId,'org.imdea.vcd.pb.Proto.MessageSet', newBuilder,[]),
    MgbMsgStatus = java:call(MgbMsgSetBuilder, setStatusValue, [0]),
    MgbMsgAdded = java:call(MgbMsgStatus, addAllMessages, [MsgList]),
    MessageSet = java:call(MgbMsgAdded, build, []),
    java:call(Socket,send,[MessageSet]),
    RcvBytes1 = java:call(java:call(java:call(java:call(MessageSet,getMessagesList,[]),get,[0]),getData,[]),toByteArray,[]),
    io:format("sending ~p~n",[java:array_to_list(RcvBytes1)]).


runRecv() ->
    {_, Socket} = init(),
    recv(Socket).


runSend() ->
    {NodeId, Socket} = init(),
    MsgList = java:new(NodeId, 'java.util.ArrayList', []),
    Message1 = java:call_static(NodeId,'org.imdea.vcd.Generator', message,["Message 1"]),  
    java:call(MsgList, add, [Message1]),
    consumeLocally(NodeId, Socket, MsgList).

