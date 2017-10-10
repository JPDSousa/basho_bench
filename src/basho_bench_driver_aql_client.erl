%%%-------------------------------------------------------------------
%%% @author joao
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. ago 2017 17:29
%%%-------------------------------------------------------------------
-module(basho_bench_driver_aql_client).
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
  Ip = lists:nth(Nth, Actors),
  AntidoteNodeStr = lists:concat(["antidote@", Ip]),
  AntidoteNode = list_to_atom(AntidoteNodeStr),
  case net_adm:ping(AntidoteNode) of
    pang ->
      lager:error("~s is not available", [AntidoteNode]),
      {error, "Connection error", #state{actor = undefined}};
    pong ->
      lager:info("worker ~b is bound to ~s", [Id, AntidoteNode]),
      case Id of
        1 ->
          create_schema(AntidoteNode);
        2 ->
          create_schema(AntidoteNode);
        3 ->
          create_schema(AntidoteNode);
        _Else -> ok
      end,
      {ok, #state{actor = AntidoteNode}}
  end.

run(get, KeyGen, ValGen, #state{actor=Node} = State) ->
  ?DEBUG("get", []),
  Key = KeyGen(),
  KeyStr = create_key(Key),
  Value = ValGen(),
  Table = integer_to_table(Value, undefined, undefined),
  Query = lists:concat(["SELECT * FROM ", Table, " WHERE Name = ", KeyStr]),
  case exec(Node, Query) of
    {ok, _} ->
      {ok, State};
    Reason ->
      ?ERROR("Error in select query: ~p", [Reason]),
      {ok, State}
  end;
run(put, KeyGen, ValGen, #state{actor=Node, artists=Artists, albums=Albums} = State) ->
  ?INFO("Put", []),
  Key = KeyGen(),
  KeyStr = create_key(Key),
  Value = ValGen(),
  ?DEBUG("Value: ~p", [Value]),
  Table = integer_to_table(Value, Artists, Albums),
  Values = gen_values(KeyStr, Table, Artists, Albums),
  Query = lists:concat(["INSERT INTO ", Table, " VALUES ", Values]),
  case exec(Node, Query) of
    {ok, _} ->
      {NewArtists, NewAlbums} = put_value(Table, Key, Artists, Albums),
      {ok, State#state{artists=NewArtists, albums=NewAlbums}};
    Err ->
      lager:error("Error in insert query: ~p", [Err]),
      {ok, State}
  end;
run(delete, KeyGen, ValGen, #state{actor=Node, artists=Artists, albums=Albums} = State) ->
  ?INFO("Delete", []),
  Key = KeyGen(),
  KeyStr = create_key(Key),
  Value = ValGen(),
  Table = integer_to_table(Value, Artists, Albums),
  Query = lists:concat(["DELETE FROM ", Table, " WHERE Name = ", KeyStr]),
  case exec(Node, Query) of
    {ok, _} ->
      {NewArtists, NewAlbums} = del_value(Table, Key, Artists, Albums),
      {ok, State#state{artists=NewArtists, albums=NewAlbums}};
    Err ->
      lager:error("Error in delete query: ~p", [Err]),
      {ok, State}
  end;
run(get_all, _KeyGen, ValGen, #state{actor=Node} = State) ->
  ?INFO("Select all", []),
  Table = integer_to_table(ValGen(), undefined, undefined),
  Query = lists:concat(["SELECT * FROM ", Table]),
  case exec(Node, Query) of
    {ok, _} ->
      {ok, State};
    Err ->
      lager:error("Error in select all query: ~p", [Err]),
      {error, Err, State}
  end;
run(Op, _KeyGen, _ValGen, _State) ->
  lager:warning("Unrecognized operation: ~p", [Op]).

exec(AntidoteNode, Query) ->
  aqlparser:parse({str, Query}, AntidoteNode).

create_schema(AntidoteNode) ->
  ArtistsQuery = "CREATE @AW TABLE Artist (Name VARCHAR PRIMARY KEY);",
  AlbumsQuery = "CREATE @AW TABLE Album (Title VARCHAR PRIMARY KEY, Artist VARCHAR FOREIGN KEY @FR REFERENCES Artist(Name));",
  TracksQuery = "CREATE @AW TABLE Track (Title VARCHAR PRIMARY KEY, Album VARCHAR FOREIGN KEY @FR REFERENCES Album(Title));",
  aqlparser:parse({str, ArtistsQuery}, AntidoteNode),
  aqlparser:parse({str, AlbumsQuery}, AntidoteNode),
  aqlparser:parse({str, TracksQuery}, AntidoteNode).

create_key(Key) ->
  lists:concat(["'", integer_to_list(Key), "'"]).

put_value("Artist", Key, Artists, Albums) ->
  {[Key | Artists], Albums};
put_value("Album", Key, Artists, Albums) ->
  {Artists, [Key | Albums]};
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

integer_to_table(1, _, _) -> "Artist";
integer_to_table(2, [], _) -> "Artist";
integer_to_table(2, _, _) -> "Album";
integer_to_table(3, [], []) -> "Artist";
integer_to_table(3, _, []) -> "Album";
integer_to_table(3, _, _) -> "Track".
