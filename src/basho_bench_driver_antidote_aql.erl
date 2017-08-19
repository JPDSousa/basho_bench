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

-include_lib("basho_bench/include/basho_bench.hrl").

-record(state, {actor, artists, albums}).


%% ====================================================================
%% API
%% ====================================================================

new(Id) ->
  Actors = basho_bench_config:get(aql_actors, []),
  Nth    = (Id - 1) rem length(Actors) + 1,
  {Name, Node} = Actor = lists:nth(Nth, Actors),
  case net_adm:ping(Node) of
    pang ->
      lager:error("~s is not available", [Node]),
      {ok, #state{actor = undefined, artists = [], albums = []}};

    pong ->
      lager:info("worker ~b is bound to ~s on ~s", [Id, Name, Node]),
      {ok, #state{actor = Actor}}
  end.

run(get, KeyGen, _ValGen, #state{actor={_Name, Node}} = State) ->
  Key = KeyGen(),
  KeyStr = integer_to_list(Key),
  Table = integer_to_table(Key rem 2, undefined, undefined),
  Query = lists:concat(["SELECT * FROM ", Table, " WHERE Name = ", KeyStr]),
  exec(Node, Query),
  {ok, State};
run(put, KeyGen, _ValGen, #state{actor={_Name, Node}, artists=Artists, albums=Albums} = State) ->
  Key = KeyGen(),
  KeyStr = integer_to_list(Key),
  Table = integer_to_table(Key rem 2, Artists, Albums),
  Values = gen_values(KeyStr, Table, Artists, Albums),
  Query = lists:concat(["INSERT INTO ", Table, " VALUES ", Values]),
  case exec(Node, Query) of
    {ok, _} ->
      {NewArtists, NewAlbums} = put_value(Table, Key, Artists, Albums),
      {ok, State#state{artists=NewArtists, albums=NewAlbums}};
    {err, Err} ->
      lager:error("Error in insert query: ~p", [Err]),
      {ok, State}
  end,
  {ok, State};
run(delete, KeyGen, _ValGen, #state{actor={_Name, Node}, artists=Artists, albums=Albums} = State) ->
  Key = KeyGen(),
  KeyStr = integer_to_list(Key),
  Table = integer_to_table(Key rem 2, Artists, Albums),
  Query = lists:concat(["DELETE FROM ", Table, " WHERE Name = ", KeyStr]),
  case exec(Node, Query) of
    {ok, _} ->
      {NewArtists, NewAlbums} = del_value(Table, Key, Artists, Albums),
      {ok, State#state{artists=NewArtists, albums=NewAlbums}};
    {err, Err} ->
      lager:error("Error in delete query: ~p", [Err]),
      {ok, State}
  end.

exec(Node, Query) ->
  rpc:call(Node, aqlparser, parse, [{str, Query}]).

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
  lists:concat(["(", Key, ", ", Artist, ")"]);
gen_values(Key, "Track", _, [Album | _Albums]) ->
  lists:concat(["(", Key, ", ", Album, ")"]).

integer_to_table(0, _, _) -> "Artist";
integer_to_table(1, [], _) -> "Artist";
integer_to_table(1, _, _) -> "Album";
integer_to_table(2, [], []) -> "Artist";
integer_to_table(2, _, []) -> "Album";
integer_to_table(2, _, _) -> "Track".

