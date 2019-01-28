Degradable objects in Antidote DB
===

### Overview
In a large scale applications such as social media applications or multiplayer online games, everyone must see roughly the same state depending on the application’s type.

But when the number of users become relatively big, the load is too high and the furthest users suffer from network delay. 

A solution is to distribute the database over several geographic locations close to its users.

As explained in the CAP theorem, a distributed data store that tolerate network delays and failures can’t be at the same time consistent and available. So generally, we have two main types of distributed databases, strongly consistent databases and eventually consistent databases.

This is a brief comparative array of the two approaches:
|             | Strongly consistent database | Eventually consistent databse |
|:-----------:|:----------------------------:|:-----------------------------:|
|   Latency   |             High             |              Low              |
| Error prone |             False            |              True             |
|  Efficiency  |          Unefficient         |           Efficient           |
| Stale data  |          Not permitted          |            Permitted           |

`Antidote DB` constitutes a sweet spot between these two approaches, it takes a new approach called just right consistency. It means that the database adapts to the consistency needs of one specific application. Antidote avoids anomalies thanks to the following features:
- It merges concurrent operations according to their semantics.
- It groups related operations in two atomic transactions and it delivers updates in causal order. 

This project aims to add a total ordering protocol that links the nodes of `Antidote DB` to ensure strong consistency as many applications require that in some operations.

#### Example: Voting application
CRDTs and synchronization-free approach could be used in a voting application before closing the vote and sealing the final result as reading real-time voting results during the vote are flexible and may vary. But this couldn't be the case when sealing the final result as it requires consensus between nodes to calculate and diffuse the final correct result that could be then read in every node.

#### VCD protocol
The `VCD` broadcast protocol is a new leaderless consensus algorithm combining free reads and exploiting commutative operations.

It can ensure strong consistency between `Antidote DB`'s replicas as it offers a total order message broadcasting.

The `VCD` broadcast protocal is written in `Erlang` and is available in the following private `Github` [repo](https://github.com/vitorenesduarte/VCD-broadcast).

#### VCD-Java-Client
The [VCD-java-client](https://github.com/vitorenesduarte/VCD-java-client) interfaces with the `VCD` protocol and allows to create sockets, to send and to receive protobuf messages, also it allows to configure `Zookeeper`.

### Solution
#### JavaErlang Binding
To interface with the `VCD` protocol from `Antidote DB` using the `Java-Client` we had to find a library that provides a binding between the two programming languages `Java` and `Erlang` as `Antidote DB` is written in `Erlang`.

[JavaErlang](https://github.com/fredlund/JavaErlang) is a binding library coded on top of [Jinterface](http://erlang.org/doc/apps/jinterface/jinterface_users_guide.html)  to make the binding easier and to facilitate the communication between `Java` and `Erlang` nodes.


#### VCD connector
- Module

    The [vcd_connector.erl](./vcd_connector.erl) module uses the `JavaErlang` library as a binding between `Erlang` and `Java` and uses the `Java-Client` to interface with the `VCD` protocol.
    
    The module provides functions to establish connection, send/receive messages to/from the `VCD` protocol and to close the connection.
    - Dependencies
        - [JavaErlang binding library](https://github.com/fredlund/JavaErlang)
        - Jar file of the [VCD-java-client](https://github.com/vitorenesduarte/VCD-java-client) Master branch

    - Usage
        1. Run the `localTesting.sh` script in terminal 1
        ```
        $ ./localTesting.sh
        ```
        2. Make sure the Jar file of the VCD-java-client named `vcd.jar` exists in the current directory
        3. In terminal 2, run an infinite receiving loop
        ```
        $ erl -sname loopRcv
        > c(vcd_connector).
        > vcd_connector:run_recv().
        ```
        4. In terminal 3, send messages
        ```
        $ erl -sname loopSend
        > c(vcd_connector).
        > vcd_connector:run_send().
- Tests

    Tests are provided in the [test.erl](./test.erl) module which uses the standard unit-testing `Erlang` library `eunit`.
    ```
    $ erlc vcd_connector.erl test.erl
    $ erl -sname test -noshell -pa ebin -eval "test:test()" -s init stop
    ```
#### Docker Environment
To simulate the behaviour of `Antidote DB` with `VCD` protocol in a replicated architecture, the idea is to run multiple docker containers of `VCD` nodes, a `Zookeeper` container to configure the `VCD` nodes, and `Antidote DB` replicas, and then link them to each others in the same docker network.
- Antidote DB
`Antidote DB` has already a `Dockerfile`, but we had to make few changes to add the `JavaErlang` library and to `COPY` the `Antidote DB` code instead of cloning it from the `Github`.
    - Dockerfile
    ```Dockerfile
    FROM erlang:19

    ENV HANDOFF_PORT "8099"
    ENV PB_PORT "8087"
    ENV PB_IP "0.0.0.0"
    ENV PBSUB_PORT "8086"
    ENV LOGREADER_PORT "8085"
    ENV RING_STATE_DIR "data/ring"
    ENV PLATFORM_DATA_DIR "data"
    ENV NODE_NAME "antidote@127.0.0.1"
    ENV SHORT_NAME "false"

    RUN apt-get update

    # Install Java 8
    RUN set -ex && \
      echo 'deb http://deb.debian.org/debian jessie-backports main' \
      > /etc/apt/sources.list.d/jessie-backports.list && \
  apt update -y && \
  apt install -t \
  jessie-backports \
  openjdk-8-jdk \
  ca-certificates-java -y

    WORKDIR /usr/src/antidote
    COPY . /usr/src/antidote

    RUN set -xe \
    && apt-get update \
    && apt-get install -y --no-install-recommends       git openssl ca-certificates

    RUN apt-get install erlang-jinterface && \
  cp -R /usr/lib/erlang/lib/jinterface-1.5.10 /usr/local/lib/erlang/lib/

    # Install JavaErlang
    RUN git clone "https://github.com/fredlund/JavaErlang.git" && \
  cd JavaErlang && \
  rebar3 compile && \
  echo y | erl -noshell -pa _build/default/lib/java_erlang/ebin -eval "java_erlang_install:install()" -s init stop && \
  cd ..

    RUN  make clean \
  && make rel \
  && cp -R _build/default/rel/antidote /opt/ \
  && sed -e '$i,{kernel, [{inet_dist_listen_min, 9100}, {inet_dist_listen_max, 9100}]}' /usr/src/antidote/_build/default/rel/antidote/releases/0.0.1/sys.config > /opt/antidote/releases/0.0.1/sys.config \
  && rm -rf /var/lib/apt/lists/*

    ADD Dockerfiles/start_and_attach.sh /opt/antidote/
    ADD Dockerfiles/entrypoint.sh /

    RUN chmod a+x /opt/antidote/start_and_attach.sh \
  && chmod a+x /entrypoint.sh

    # Distributed Erlang Port Mapper
    EXPOSE 4369
    # Ports for Antidote
    EXPOSE 8085 8086 8087 8099

    # Antidote RPC
    EXPOSE 9100

    VOLUME /opt/antidote/data

    ENTRYPOINT ["/entrypoint.sh"]

    CMD ["/opt/antidote/start_and_attach.sh"]
    ```
    - .dockerignore
    ```.dockerignore
    **/*.beam
    **/*.rebar3
    **/rebar.lock
    _build/*
    ```
    - rebar.config
    ```rebar.config
    %% binding java - erlang 
    {java_erlang, {git, "git://github.com/fredlund/JavaErlang.git", {ref, "a13dd22d0f"}}
    ```
    Build an `Antidote DB` docker image
    ```shell
    $ docker build -f Dockerfiles/Dockerfile -t antidotedb/antidote .
    ```
- VCD nodes with Zookeeper
The `VCD` nodes are dockerized and can be pulled from `dockerhub`, `kooli94/vcd:working_version`.
what we need is to configure them to connect to a Zookeeper container, please see the following [docker-compose.yml](./docker-compose.yml) file.
- Docker compose

    The [docker-compose.yml](./docker-compose.yml) file defines a docker network named `default_ntwk` and configure 3 `VCD` nodes (3 containers) and a `Zookeeper` container to connect to that network.
    - docker-compose.yml
    ```yml
    version: "3"
    services:
      zk:
        image: zookeeper
        expose:
          - 2181
        networks:
          default_ntwk:
            ipv4_address: 10.5.0.99
      vcd0:
        image: kooli94/vcd:working_version
        environment:
          - ZK=10.5.0.99
          - IP=10.5.0.100
          - ID=0
          - NODE_NUMBER=3
          - HPORT=5000
          - CPORT=6000
          - POOL_SIZE=1
        expose:
          - 5000
          - 6000
        networks:
          default_ntwk:
            ipv4_address: 10.5.0.100
        depends_on:
          - zk
      vcd1:
        image: kooli94/vcd:working_version
        environment:
          - ZK=10.5.0.99
          - IP=10.5.0.101
          - ID=1
          - NODE_NUMBER=3
          - HPORT=5000
          - CPORT=6000
          - POOL_SIZE=1
        expose:
          - 5000
          - 6000
        networks:
          default_ntwk:
            ipv4_address: 10.5.0.101
        depends_on:
          - zk
      vcd2:
        image: kooli94/vcd:working_version
        environment:
          - ZK=10.5.0.99
          - IP=10.5.0.102
          - ID=2
          - NODE_NUMBER=3
          - HPORT=5000
          - CPORT=6000
          - POOL_SIZE=1
        expose:
          - 5000
          - 6000
        networks:
          default_ntwk:
            ipv4_address: 10.5.0.102
        depends_on:
          - zk

    networks:
      default_ntwk:
        driver: bridge
        ipam:
          config:
            - subnet: 10.5.0.0/16
    ```
    In order to run and connect `Antidote DB` to the `default_ntwk` docker network we add the `--network antidote-draft_default_ntwk` flag and we pass the `Zookeeper` port number and address through an environment variable `ZK` to `Antidote DB` as it's going to be used by `vcd_connector` later to connect to `VCD` nodes using the flag `-e ZK="-zk=10.5.0.99:2181"`.
    
    To run an connect an `Antidote DB` container to the docker network
    ```shell
    $ docker run -i -t -d --name antidote1 --network antidote-draft_default_ntwk -e SHORT_NAME=true -e NODE_NAME=antidote@antidote1 -e ZK="-zk=10.5.0.99:2181" antidotedb/antidote
    ```
### VCD integration in Antidote DB
#### Antidote DB project tree
`Antidote DB` uses two `Erlang` behaviour modules: `supervisor` and `gen_server`.

**Supervisor**: Responsible for starting, stopping, and monitoring its child processes. The basic idea of a supervisor is that it is to keep its child processes alive by restarting them when necessary.

**Gen_server**: provides the server of a client-server architecture. A module with `gen_server` behaviour must implement a set of callback functions to define relationship with the behaviour functions.

![](https://i.imgur.com/a0WrDbI.png)


`Antidote DB` fetch and build its dependencies recurcively using [rebar3](https://www.rebar3.org/).
List of dependencies:
[Erlzmq](https://github.com/zeromq/erlzmq2): Efficient inter-dc messaging 
[antidote_crdt](https://github.com/AntidoteDB/antidote_crdt): Operation based `CRDT` implementations
[antidote_pb_codec](https://github.com/AntidoteDB/antidote_pb_codec): Defines the protocol buffer interface of `Antidote`
[Vectorclock](https://github.com/AntidoteDB/vectorclock): A vector clock library for `Erlang`.
#### inter_dc modules
The communication's logic between multiple `Antidote DB` replicas is written in the `src/inter_dc_*` modules and based on [zmq](http://zeromq.org/) queuing system with [erlzmq](https://github.com/zeromq/erlzmq2) `Erlang` library to exchange messages between the different replicas.

Each module runs as a process with `gen_server` behaviour and supervized by `antidote_sup.erl` which runs as supervisor, these modules provide to each `Antidote DB` replica functions and mechanisms to subscribe/publish to the `zmq` queuing system and to exchange states and queries between replicas.

Each module implements callback functions required by the `gen_server` behaviour like `init/1` to create socket and `handle_call/3` to handle incoming requests.
#### Issues
- `Antidote DB` crashes with an **undefined error message** when calling functions from `JavaErlang` binding library.

- Is changing `erlzmq` calls in `inter_dc_*` modules by `vcd_connector`'s functions the best approach to integrate the `VCD` broadcast protocol in `Antidote DB` ? As the current `inter dc` communication is based on a queuing system not a client-server approach like `VCD-java-client`.