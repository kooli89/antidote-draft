-module(scriptLoop).
-export([runSend/0, runRecv/0]).


recv(Socket) ->
    RcvSet = java:call(Socket,'receive',[]),
    RcvBytes = java:call(java:call(java:call(java:call(RcvSet,getMessagesList,[]),get,[0]),getData,[]),toByteArray,[]),
    io:format("received ~p~n",[java:array_to_list(RcvBytes)]),
    timer:sleep(1000),
    recv(Socket).


consumeLocally(NodeId, Socket) ->
    MsgList = java:new(NodeId, 'java.util.ArrayList', []),
    Message1 = java:call_static(NodeId,'org.imdea.vcd.Generator', message,["Message 1"]),
    Message2 = java:call_static(NodeId,'org.imdea.vcd.Generator', message,["Message 2"]),
    Message3 = java:call_static(NodeId,'org.imdea.vcd.Generator', message,["Message 3"]),    
    Message4 = java:call_static(NodeId,'org.imdea.vcd.Generator', message,["Message 4"]),    
    java:call(MsgList, add, [Message1]),
    java:call(MsgList, add, [Message2]),
    java:call(MsgList, add, [Message3]),
    java:call(MsgList, add, [Message4]),

    Array = java:call(MsgList, toArray, []),
    io:format("Message List: ~p~n", [java:array_to_list(Array)]),

    MgbMsgSetBuilder = java:call_static(NodeId,'org.imdea.vcd.pb.Proto.MessageSet', newBuilder,[]),
    MgbMsgStatus = java:call(MgbMsgSetBuilder, setStatusValue, [0]),
    MgbMsgAdded = java:call(MgbMsgStatus, addAllMessages, [MsgList]),
    MessageSet = java:call(MgbMsgAdded, build, []),
    java:call(Socket,send,[MessageSet]).


runRecv() ->
    application:ensure_started(java_erlang),
    java:set_timeout(infinity),
    {ok,NodeId} = java:start_node([{add_to_java_classpath,["vcd.jar"]}]),
    Conf = java:call_static(NodeId,'org.imdea.vcd.Config',parseArgs,[""]),
    Socket = java:call_static(NodeId,'org.imdea.vcd.Socket',create,[Conf,10]),
    recv(Socket).


runSend() ->
    application:ensure_started(java_erlang),
    {ok,NodeId} = java:start_node([{add_to_java_classpath,["vcd.jar"]}]),
    Conf = java:call_static(NodeId,'org.imdea.vcd.Config',parseArgs,[""]),
    Socket = java:call_static(NodeId,'org.imdea.vcd.Socket',create,[Conf,10]),
    consumeLocally(NodeId, Socket).

    