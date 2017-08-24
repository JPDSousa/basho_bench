%%%-------------------------------------------------------------------
%%% @author joao
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. ago 2017 17:29
%%%-------------------------------------------------------------------
-module(basho_bench_driver_antidote_aql).
-author("joao").

-export([new/1,
  run/4]).

-include_lib("basho_bench.hrl").

-record(state, {actor, artists = [], albums = []}).


%% ====================================================================
%% API
%% ====================================================================

new(Id) ->
  Actors = basho_bench_config:get(aql_actors, []),
  Nth    = (Id - 1) rem length(Actors) + 1,
  {Name, AQLNode, AntidoteNode} = lists:nth(Nth, Actors),
  case net_adm:ping(AQLNode) of
    pang ->
      lager:error("~s is not available", [AQLNode]),
      {error, "Connection error", #state{actor = undefined}};

    pong ->
      lager:info("worker ~b is bound to ~s on ~s", [Id, Name, AQLNode]),
      {ok, #state{actor = {Name, {AQLNode, AntidoteNode}}}}
  end.

run(get, KeyGen, ValGen, #state{actor={_Name, Node}} = State) ->
  ?DEBUG("get: begin", []),
  Key = KeyGen(),
  KeyStr = create_key(Key),
  Value = ValGen(),
  Table = integer_to_table(Value, undefined, undefined),
  Query = lists:concat(["SELECT * FROM ", Table, " WHERE Name = ", KeyStr]),
  ?DEBUG("get: query->", [Query]),
  Res = exec(Node, Query),
  ?DEBUG("get: res->~p", [Res]),
  case Res of
    {ok, _} ->
      {ok, State};
    {err, Reason} ->
      ?ERROR("Error in select query: ~p", [Reason]),
      {error, Reason, State}
  end;
run(put, KeyGen, ValGen, #state{actor={_Name, Node}, artists=Artists, albums=Albums} = State) ->
  ?INFO("Put", []),
  Key = KeyGen(),
  KeyStr = create_key(Key),
  Value = ValGen(),
  Table = integer_to_table(Value, Artists, Albums),
  Values = gen_values(KeyStr, Table, Artists, Albums),
  Query = lists:concat(["INSERT INTO ", Table, " VALUES ", Values]),
  ?DEBUG("put query: ~p", [Query]),
  case exec(Node, Query) of
    {ok, _} ->
      {NewArtists, NewAlbums} = put_value(Table, Key, Artists, Albums),
      {ok, State#state{artists=NewArtists, albums=NewAlbums}};
    {err, Err} ->
      lager:error("Error in insert query: ~p", [Err]),
      {error, Err, State}
  end;
run(delete, KeyGen, ValGen, #state{actor={_Name, Node}, artists=Artists, albums=Albums} = State) ->
  ?INFO("Delete", []),
  Key = KeyGen(),
  KeyStr = create_key(Key),
  Value = ValGen(),
  Table = integer_to_table(Value, Artists, Albums),
  Query = lists:concat(["DELETE FROM ", Table, " WHERE Name = ", KeyStr]),
  ?DEBUG("DELETE query: ~p", [Query]),
  case exec(Node, Query) of
    {ok, _} ->
      {NewArtists, NewAlbums} = del_value(Table, Key, Artists, Albums),
      {ok, State#state{artists=NewArtists, albums=NewAlbums}};
    {err, Err} ->
      lager:error("Error in delete query: ~p", [Err]),
      {error, Err, State}
  end;
run(Op, _KeyGen, _ValGen, _State) ->
  lager:warning("Unrecognized operation: ~p", [Op]).

exec({AQLNode, AntidoteNode}, Query) ->
  rpc:call(AQLNode, aqlparser, parse, [{str, Query}, AntidoteNode]).

create_key(Key) ->
  lists:concat(["'", integer_to_list(Key), "'"]).

put_value("Artist", Key, Artists, Albums) ->
  {Artists ++ [Key], Albums};
put_value("Album", Key, Artists, Albums) ->
  {Artists, Albums ++ [Key]};
put_value("Track", _Key, Artists, Albums) ->
  {Artists, Albums}.

del_value("Artist", Key, Artists, Albums) ->
  {lists:delete(Key, Artists), Albums};
del_value("Album", Key, Artists, Albums) ->
  {Artists, lists:delete(Key, Albums)};
del_value("Track", _Key, Artists, Albums) ->
  {Artists, Albums}.

gen_values(Key, "Artist", _, _) ->
  lists:concat(["(", Key, ")"]);
gen_values(Key, "Album", [Artist | _Artists], _) ->
  lists:concat(["(", Key, ", '", Artist, "')"]);
gen_values(Key, "Track", _, [Album | _Albums]) ->
  lists:concat(["(", Key, ", '", Album, "')"]).

integer_to_table(0, _, _) -> "Artist";
integer_to_table(1, [], _) -> "Artist";
integer_to_table(1, _, _) -> "Album";
integer_to_table(2, [], []) -> "Artist";
integer_to_table(2, _, []) -> "Album";
integer_to_table(2, _, _) -> "Track".

