%%%===================================================================
%%% @author Rafał Studnicki <rafal@opencubeware.org>
%%% @copyright (c) 2013 opencubeware.org
%%% @doc Public behaviour 
%%% @end
%%%===================================================================

-module(chameleon).

-include("chameleon.hrl").

-export([records/1,
         input_filter/2,
         output_filter/2,
         json/1,
         json/2,
         proplist/1,
         record/1,
         record/2]).

-type record_name() :: atom().
-type field() :: atom().
-type filter() :: {record_name(), fun() | invalid}.
-type error() :: unprocessable | invalid.
-export_type([record_name/0, field/0, filter/0, error/0]).

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

-spec output_filter(record_name(), filter()) -> true.
output_filter(Record, Filter) ->
    add_filter(?OUTPUT_FILTERS_TABLE, Record, Filter).

-spec input_filter(record_name(), filter()) -> true.
input_filter(Record, Filter) ->
    add_filter(?INPUT_FILTERS_TABLE, Record, Filter).

-spec json(record() | proplists:proplist()) ->
      {ok, binary()} | {error, error()}.
json(Subject) ->
    json(Subject, fun(X) -> X end). 

-spec json(record() | proplists:proplist(), filter()) ->
      {ok, binary()} | {error, error()}.
json(Subject, Filter) ->
    chameleon_json:transform({json, Subject}, Filter).

-spec proplist(binary()) ->
    {ok, proplists:proplist()} | {error, error()}.
proplist(Binary) ->
    chameleon_json:transform({proplist, Binary}, fun(X) -> X end).

-spec record(binary()) ->
    {ok, proplists:proplist()} | {error, error()}.
record(Binary) ->
    record(Binary, fun(X) -> X end).

-spec record(binary(), filter()) ->
    {ok, proplists:proplist()} | {error, error()}.
record(Binary, Filter) ->
    chameleon_json:transform({record, Binary}, Filter).

%%%===================================================================
%%% Internal functions
%%%===================================================================
add_filter(Table, Record, Filter) ->
    true = ets:insert(Table, {Record, Filter}).
