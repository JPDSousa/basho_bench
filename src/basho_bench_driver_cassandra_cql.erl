%% -------------------------------------------------------------------
%%
%% basho_bench: Benchmarking Suite
%%
%% Copyright (c) 2009-2012 Basho Techonologies
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(basho_bench_driver_cassandra_cql).
-compile({inline, [run_put/3]}).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-record(state, { client,
                 keyspace,
                 columnfamily,
                 column,
                 partition_key,
                 get_query,
                 put_query,
                 put_ts_query,
                 delete_query,
                 put_composite_query,
                 get_composite_query,
                 last_row_key = 0,
                 range_query_num_rows,
                 timestamp,
                 hostname,
                 id
               }).


%% ====================================================================
%% API
%% ====================================================================

new(Id) ->
    Ips = basho_bench_config:get(cassandra_ips, [{"localhost", 9042}]),
    Port = basho_bench_config:get(cassandra_port, 9042),
    Keyspace = basho_bench_config:get(cassandra_keyspace, "Keyspace1"),
    ColumnFamily = basho_bench_config:get(cassandra_columnfamily, "ColumnFamily1"),
    ValueColumn = basho_bench_config:get(cassandra_column, "Column"),
    CompositePartitionColumn = basho_bench_config:get(cassandra_composite_partition_column, "PartitionKey"),
    CompositeRowColumn = basho_bench_config:get(cassandra_composite_row_column, "RowKey"),
    ReadConsistency = basho_bench_config:get(cassandra_read_consistency, ?CQERL_CONSISTENCY_ONE),
    WriteConsistency = basho_bench_config:get(cassandra_write_consistency, ?CQERL_CONSISTENCY_QUORUM),

    {Mega,Sec,Micro} = erlang:now(),
    NowTimestamp = (Mega*1000000 + Sec)*1000 + round(Micro/1000),
    Timestamp = basho_bench_config:get(start_timestamp, NowTimestamp),

    {ok, Hostname} = inet:gethostname(),
    
    %% connect to client
    %% Choose the target node using our ID as a modulus
    Targets = basho_bench_config:normalize_ips(Ips, Port),
    {TargetIp, TargetPort} = lists:nth((Id rem length(Targets)+1), Targets),
    ?INFO("Using target ~p:~p for worker ~p\n", [TargetIp, TargetPort, Id]),
    application:ensure_all_started(cqerl),
    {ok, C} = cqerl:new_client({TargetIp, TargetPort}),

    case ksbarrier(C, Keyspace) of
        ok ->
            %% Build parameterized, reusable queries as we assume a typical
            %% high-volume Cassandra application would.
            GetQueryText = iolist_to_binary(["SELECT ", ValueColumn," FROM ", ColumnFamily ," where KEY = :key"]),
            GetQuery = #cql_query{statement = GetQueryText, consistency = ReadConsistency},
            PutQueryText = iolist_to_binary(["UPDATE ", ColumnFamily,
                                             " SET ", ValueColumn, " = :val WHERE KEY = :key;"]),
            PutQuery = #cql_query{statement = PutQueryText, consistency = WriteConsistency},
            
            TSPutText = iolist_to_binary(["INSERT INTO ", ColumnFamily,
                                          "(myfamily, myseries, time, time_quanta, myint, mytext, myfloat, mybool) 
                                           VALUES (:myfamily, :myseries, :time, :time_quanta, :myint, :mytext, :myfloat, :mybool);"]),
            TSPutQuery = #cql_query{statement = TSPutText, consistency = WriteConsistency},
            
            DeleteQueryText = ["DELETE FROM ", ColumnFamily ," WHERE KEY = :key;"],
            DeleteQuery = #cql_query{statement = DeleteQueryText, consistency = WriteConsistency},
            GetCompositeQueryText = iolist_to_binary(["SELECT ", ValueColumn," FROM ", ColumnFamily ," WHERE ",
                                                      CompositePartitionColumn, " = :partition_key AND ",
                                                      CompositeRowColumn, " > :min_row_key AND ",
                                                      CompositeRowColumn, " < :max_row_key;"]),
            GetCompositeQuery = #cql_query{statement = GetCompositeQueryText, consistency = ReadConsistency},
            PutCompositeQueryText = iolist_to_binary(["UPDATE ", ColumnFamily,
                                                      " SET ", ValueColumn, " = :val WHERE ", CompositePartitionColumn,
                                                      " = :partition_key AND ", CompositeRowColumn ," = :row_key;"]),
            PutCompositeQuery = #cql_query{statement=PutCompositeQueryText, consistency = WriteConsistency},
            RangeQueryNumRows = basho_bench_config:get(cassandra_range_query_num_rows, 500),

            {ok, #state { client = C,
                          partition_key = io_lib:format("~p - ~p", [node(), Id]),
                          get_query = GetQuery,
                          put_query = PutQuery,
                          put_ts_query = TSPutQuery,
                          put_composite_query = PutCompositeQuery,
                          get_composite_query = GetCompositeQuery,
                          delete_query = DeleteQuery,
                          range_query_num_rows = RangeQueryNumRows,
                          timestamp = Timestamp,
                          hostname = list_to_binary(Hostname),
                          id = list_to_binary(lists:flatten(io_lib:format("~p", [Id])))}};
        {error, Reason} ->
            error_logger:error_msg("Failed to get a cqerl client for ~p: ~p\n",
                                   [TargetIp, Reason])
    end.


ksbarrier(C, Keyspace) ->
    case cqerl:run_query(C, lists:concat(["USE ", Keyspace, ";"])) of
        {ok, _KSBin} -> ok;
        {error, not_ready} ->
            %% Not ready yet, try again
            timer:sleep(100),
            ksbarrier(C, Keyspace);
        {error, _} = Error ->
            Error
    end.

run(get, KeyGen, _ValueGen,
    #state{client=C, get_query=CqlQuery}=State) ->
    Key = KeyGen(),
    ParameterizedQuery = CqlQuery#cql_query{values = [{key, Key}]},
    case cqerl:run_query(C, ParameterizedQuery) of
        {ok, #cql_result{cql_query=ParameterizedQuery} = _Result} ->
            {ok, State};
        Error ->
            {error, Error, State}
    end;
run(delete, KeyGen, _ValueGen,
    #state{client=C, delete_query=DeleteQuery}=State) ->
    Key = KeyGen(),
    ParameterizedQuery = DeleteQuery#cql_query{values = [{key, Key}]},
    case cqerl:run_query(C, ParameterizedQuery) of
        {ok,void} ->
            {ok, State};
        Error ->
            {error, Error, State}
    end;
run(put_composite, KeyGen, ValueGen,
    #state{client=C, put_composite_query = Query, partition_key = PartitionKey}=State) ->
    RowKey = KeyGen(),
    Val = ValueGen(),
    ParameterizedQuery = Query#cql_query{values = [{partition_key, PartitionKey}, {row_key, RowKey}, {val, Val}]},
    case cqerl:run_query(C, ParameterizedQuery) of
        {ok, void} ->
            {ok, State#state{last_row_key = RowKey}};
        Error ->
            {error, Error, State}
    end;
run(query_composite, KeyGen, _ValueGen,
    #state{client=C, get_composite_query = RangeQuery, partition_key = PartitionKey,
           last_row_key = LastRowKey0, range_query_num_rows = NumRows}=State) ->
    LastRowKey = case LastRowKey0 of
                     0 -> KeyGen();
                     _ -> LastRowKey0
                 end,
    ParameterizedQuery = RangeQuery#cql_query{values = [{partition_key, PartitionKey},
                                                        {min_row_key, LastRowKey - NumRows},
                                                        {max_row_key, LastRowKey}]},
    case cqerl:run_query(C, ParameterizedQuery) of
        {ok, _Result} ->
            {ok, State};
        Error ->
            {error, Error, State}
    end;


%% `insert` and `put` are functinally and performance-wise equivalent to Cassandra
%% Keeping both in order to retain backward-compatibility with other people's test cases
%% run_put is inlined.
run(insert, KeyGen, ValueGen, State) ->
    run_put(KeyGen, ValueGen, State);
run(put, KeyGen, ValueGen, State) ->
    run_put(KeyGen, ValueGen, State);

run(ts_put, _KeyGen, _ValueGen, State) ->
  C = State#state.client,
  TSPutQuery = State#state.put_ts_query,

  % Calculate the quanta, currently hardcoaded for a 15 minute quanta
  Timestamp = State#state.timestamp,
  FifteenMinutesInSeconds = 900.0,
  Quanta = trunc(trunc((float(trunc(Timestamp / 1000.0)) / FifteenMinutesInSeconds)) * FifteenMinutesInSeconds) * 1000,
  ParameterizedQuery = TSPutQuery#cql_query{values = [{myfamily, State#state.hostname}, 
                                                      {myseries, State#state.id}, 
                                                      {time, Timestamp},
                                                      {time_quanta, Quanta}, 
                                                      {myint, 1}, 
                                                      {mytext, <<"test1">>}, 
                                                      {myfloat, 1.0}, 
                                                      {mybool, true}]},

  case cqerl:run_query(C, ParameterizedQuery) of
    {ok, void} ->
      {ok, State#state{timestamp = Timestamp + 1}};
    Error ->
      io:format("Error: ~p~n", [Error]),
      {error, Error, State}
  end;

run(ts_query, KeyGen, _ValueGen, State) ->
  C = State#state.client,
  Query = KeyGen(),
  
  case cqerl:run_query(C, #cql_query{statement=Query, page_size=4096}) of
    {ok, Results} ->
      _Rows = cqerl:all_rows(Results),
      {ok, State};
    Error ->
      io:format("Error: ~p~n", [Error]),
      {error, Error, State}
    end.

run_put(KeyGen, ValueGen,#state{client=C, put_query = PutQuery}=State) ->
    Key = KeyGen(),
    Val = ValueGen(),
    ParameterizedQuery = PutQuery#cql_query{values = [{key, Key}, {val, Val}]},
    case cqerl:run_query(C, ParameterizedQuery) of
        {ok, void} ->
            {ok, State};
        Error ->
            {error, Error, State}
    end.
