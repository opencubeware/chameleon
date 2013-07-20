%%%===================================================================
%%% @author Rafał Studnicki <rafal@opencubeware.org>
%%% @copyright (c) 2013 opencubeware.org
%%% @doc Public behaviour 
%%% @end
%%%===================================================================

-module(chameleon).

-include("chameleon.hrl").

-export([records/1,
         post_filters/2,
         pre_filters/2,
         json/1,
         json/2,
         json/3,
         proplist/1,
         proplist/2,
         proplist/3,
         record/1,
         record/2,
         record/3]).

-type record_name() :: atom().
-type field() :: atom().
-type filter() :: {field(), fun()}.
-type validator() :: {field(), fun()}.
-export_type([record_name/0, field/0, filter/0, validator/0]).

%%%===================================================================
%%% Public API
%%%===================================================================
-spec records(atom() | [atom()]) -> true.
records(Modules) when is_list(Modules) ->
    Records = lists:foldl(fun(Module, Acc) ->
                    case erlang:function_exported(Module,?RECORD_FUNCTION,0) of
                        true -> Acc ++ apply(Module, ?RECORD_FUNCTION, []);
                        _ -> Acc
                    end
            end, [], Modules),
    true = ets:insert(?RECORDS_TABLE, Records);
records(Module) ->
    records([Module]).

-spec post_filters(record_name(), [filter()]) -> true.
post_filters(Record, Filters) ->
    true = ets:insert(?POST_FILTERS_TABLE, {Record, Filters}).

-spec pre_filters(record_name(), [filter()]) -> true.
pre_filters(Record, Filters) ->
    true = ets:insert(?PRE_FILTERS_TABLE, {Record, Filters}).

-spec json(record() | proplists:proplist()) ->
      {ok, binary()} | {error, atom()}.
json(Subject) ->
   json(Subject, [], []). 

-spec json(record() | proplists:proplist(), [filter()]) ->
      {ok, binary()} | {error, atom()}.
json(Subject, Filters) ->
    json(Subject, Filters, []).

-spec json(record() | proplists:proplist(), [filter()], [validator()]) ->
      {ok, binary()} | {error, atom()}.
json(Subject, Filters, Validators) ->
    chameleon_json:transform({json, Subject}, Filters, Validators).

-spec proplist(binary()) ->
    {ok, proplists:proplist()} | {error, atom()}.
proplist(Binary) ->
    proplist(Binary, [], []).

-spec proplist(binary(), [filter()]) ->
    {ok, proplists:proplist()} | {error, atom()}.
proplist(Binary, Filters) ->
    proplist(Binary, Filters, []).

-spec proplist(binary(), [filter()], [validator()]) ->
    {ok, proplists:proplist()} | {error, atom()}.
proplist(Binary, Filters, Validators) ->
    chameleon_json:transform({proplist, Binary}, Filters, Validators).

-spec record(binary()) ->
    {ok, proplists:proplist()} | {error, atom()}.
record(Binary) ->
    record(Binary, [], []).

-spec record(binary(), [filter()]) ->
    {ok, proplists:proplist()} | {error, atom()}.
record(Binary, Filters) ->
    record(Binary, Filters, []).

-spec record(binary(), [filter()], [validator()]) ->
    {ok, proplists:proplist()} | {error, atom()}.
record(Binary, Filters, Validators) ->
    chameleon_json:transform({record, Binary}, Filters, Validators).
