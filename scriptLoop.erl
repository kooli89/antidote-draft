-module(scriptLoop).
-export([send/6, recv/3, run/0]).


send(0,_,_,_,_,_) -> 
    [];

    send (N, Term, Key, Conflicts, NodeId, Socket) when N > 0 ->
        Data = java:call_static(NodeId, 'com.google.protobuf.ByteString', copyFrom, [lists:flatten(io_lib:format("~p", [N]))]),
        SentSet = java:call_static(NodeId,'org.imdea.vcd.Generator', messageSet,[Key, Conflicts, Data]),
        SentBytes = java:call(java:call(java:call(java:call(SentSet,getMessagesList,[]),get,[0]),getData,[]),toByteArray,[]),
        io:format("sending ~p~n",[java:array_to_list(SentBytes)]),
        java:call(Socket,send,[SentSet]),
        timer:sleep(2000),
        [Term|send(N-1,Term, Key, Conflicts, NodeId, Socket)]. 
 
recv(0,_,_) ->
    [];

    recv(N, Term, Socket) when N > 0 ->
        RcvSet = java:call(Socket,'receive',[]),
        RcvBytes = java:call(java:call(java:call(java:call(RcvSet,getMessagesList,[]),get,[0]),getData,[]),toByteArray,[]),
        io:format("received ~p~n",[java:array_to_list(RcvBytes)]),
        timer:sleep(2000),
        [Term|recv(N-1,Term,Socket)].

consumeLocally(NodeId, Socket) ->
    MsgList = java:new(NodeId, 'java.util.ArrayList', []),
    Message1 = java:call_static(NodeId,'org.imdea.vcd.Generator', message,["Message 1"]),
    Message2 = java:call_static(NodeId,'org.imdea.vcd.Generator', message,["Message 2"]),
    Message3 = java:call_static(NodeId,'org.imdea.vcd.Generator', message,["Message 3"]),    
    java:call(MsgList, add, [Message1]),
    java:call(MsgList, add, [Message2]),
    java:call(MsgList, add, [Message3]),
    Array = java:call(MsgList, toArray, []),
    io:format("Message List: ~p~n", [java:array_to_list(Array)]),

    MgbMsgSetBuilder = java:call_static(NodeId,'org.imdea.vcd.pb.Proto.MessageSet', newBuilder,[]),
    MgbMsgStatus = java:call(MgbMsgSetBuilder, setStatusValue, [0]),
    MgbMsgAdded = java:call(MgbMsgStatus, addAllMessages, [MsgList]),
    MessageSet = java:call(MgbMsgAdded, build, []),
    java:call(Socket,send,[MessageSet]),

    RcvSet = java:call(Socket,'receive',[]),
    RcvBytes = java:call(java:call(java:call(java:call(RcvSet,getMessagesList,[]),get,[0]),getData,[]),toByteArray,[]),
    io:format("received ~p~n",[java:array_to_list(RcvBytes)]).




run() ->
    application:ensure_started(java_erlang),
    {ok,NodeId} = java:start_node([{add_to_java_classpath,["vcd.jar"]}]),
    Conf = java:call_static(NodeId,'org.imdea.vcd.Config',parseArgs,[""]),
    Socket = java:call_static(NodeId,'org.imdea.vcd.Socket',create,[Conf,10]),

    Conflicts = java:new(NodeId, 'java.lang.Integer', [10]),
    Key = java:call_static(NodeId, 'com.google.protobuf.ByteString', copyFrom, ["key"]),

    % send(10, 1, Key, Conflicts, NodeId, Socket),
    % recv(22, 1, Socket).
    consumeLocally(NodeId, Socket).