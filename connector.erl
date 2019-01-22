-module(connector).
-export([init/0, runSend/0, runRecv/0, recvOneMsg/1,send/3]).

init() ->
    application:ensure_started(java_erlang),
    java:set_timeout(infinity),
    {ok,NodeId} = java:start_node([{add_to_java_classpath,["vcd.jar"]}]),
    ZkIP = os:getenv("ZK", "-zk=127.0.0.1:2181"), %% default value is 127.0.0.1:2181 if ZK env var is not set, ZK should respect the format -zk=ip:port
    Conf = java:call_static(NodeId,'org.imdea.vcd.Config',parseArgs,[[ZkIP]]),
    Socket = java:call_static(NodeId,'org.imdea.vcd.Socket',create,[Conf,10]),
    {NodeId, Socket}.


recvOneMsg(Socket) ->
    RcvSet = java:call(Socket,'receive',[]),
    RcvMsg = java:call(java:call(RcvSet,getMessagesList,[]),get,[0]),
    % io:format("received ~p~n",[java:array_to_list(RcvBytes)]),
    Status = java:call(RcvSet, getStatusValue, []),
    % io:format("received status ~p~n",[Status]),
    if Status == 2 ->
        {RcvMsg, Status};
    true -> 
        recvOneMsg(Socket)
    end.


recv(Socket) ->
    RcvMsg = recvOneMsg(Socket),
    % io:format("received ~p~n",[java:array_to_list(RcvBytes)]),
    timer:sleep(1000),
    recv(Socket).


send(NodeId, Socket, MsgList) ->
    Array = java:call(MsgList, toArray, []),
    % io:format("Message List: ~p~n", [java:array_to_list(Array)]),

    MgbMsgSetBuilder = java:call_static(NodeId,'org.imdea.vcd.pb.Proto.MessageSet', newBuilder,[]),
    MgbMsgStatus = java:call(MgbMsgSetBuilder, setStatusValue, [0]),
    MgbMsgAdded = java:call(MgbMsgStatus, addAllMessages, [MsgList]),
    MessageSet = java:call(MgbMsgAdded, build, []),
    java:call(Socket,send,[MessageSet]),
    SendMsg = java:call(java:call(MessageSet,getMessagesList,[]),get,[0]),
    % io:format("sending ~p~n",[java:array_to_list(SendBytes)]),
    SendMsg.


runRecv() ->
    {_, Socket} = init(),
    recv(Socket).


runSend() ->
    {NodeId, Socket} = init(),
    MsgList = java:new(NodeId, 'java.util.ArrayList', []),
    Message1 = java:call_static(NodeId,'org.imdea.vcd.Generator', message,["Message 1"]),  
    java:call(MsgList, add, [Message1]),
    send(NodeId, Socket, MsgList).

