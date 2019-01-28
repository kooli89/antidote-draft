-module(vcd_connector).
-export([init/0, run_send/0, run_recv/0, recv_one_msg/1, send/3]).


%% @doc This function initializes the connection between Erlang and VCD-JavaClient
%% Returns a Java node id and a Socket object
init() ->
    application:ensure_started(java_erlang),
    java:set_timeout(infinity),
    {ok,NodeId} = java:start_node([{add_to_java_classpath,["vcd.jar"]}]), %% TODO put the vcd file path in a config file
    ZkIP = os:getenv("ZK", "-zk=127.0.0.1:2181"), %% default value is 127.0.0.1:2181 if ZK env var is not set, ZK should respect the format -zk=ip:port
    Conf = java:call_static(NodeId,'org.imdea.vcd.Config',parseArgs,[[ZkIP]]),
    Socket = java:call_static(NodeId,'org.imdea.vcd.Socket',create,[Conf,10]),
    {NodeId, Socket}.


%% @doc This function takes a Socket as argument and
%% receives one message from the VCD protocol
%% Returns the message and its status
recv_one_msg(Socket) ->
    RcvSet = java:call(Socket,'receive',[]),
    RcvMsg = java:call(java:call(RcvSet,getMessagesList,[]),get,[0]),
    Status = java:call(RcvSet, getStatusValue, []),
    {RcvMsg, Status}.


%% @doc This function takes a Socket as argument and
%% receives messages from the VCD protocol recursively
recv(Socket) ->
    _ = recv_one_msg(Socket),
    timer:sleep(1000),
    recv(Socket).


%% @doc This function takes a Node id, Socket, List of messages as arguments and
%% sends the list of messages to the VCD protocol
%% Returns the sent message
send(NodeId, Socket, MsgList) ->
    _ = java:call(MsgList, toArray, []),
    MgbMsgSetBuilder = java:call_static(NodeId,'org.imdea.vcd.pb.Proto.MessageSet', newBuilder,[]),
    MgbMsgStatus = java:call(MgbMsgSetBuilder, setStatusValue, [0]),
    MgbMsgAdded = java:call(MgbMsgStatus, addAllMessages, [MsgList]),
    MessageSet = java:call(MgbMsgAdded, build, []),
    java:call(Socket,send,[MessageSet]),
    SendMsg = java:call(java:call(MessageSet,getMessagesList,[]),get,[0]),
    SendMsg.


%% @doc This function runs the recv function
run_recv() ->
    {_, Socket} = init(),
    recv(Socket).


%% @doc This function runs the send function
run_send() ->
    {NodeId, Socket} = init(),
    MsgList = java:new(NodeId, 'java.util.ArrayList', []),
    Message1 = java:call_static(NodeId,'org.imdea.vcd.Generator', message,["Message 1"]),  
    java:call(MsgList, add, [Message1]),
    send(NodeId, Socket, MsgList).

