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
         json/3]).

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

-spec post_filters(atom(), proplists:proplist()) -> true.
post_filters(Record, Filters) ->
    true = ets:insert(?POST_FILTERS_TABLE, {Record, Filters}).

-spec pre_filters(atom(), proplists:proplist()) -> true.
pre_filters(Record, Filters) ->
    true = ets:insert(?PRE_FILTERS_TABLE, {Record, Filters}).

-spec json(record() | proplists:proplist()) ->
      {ok, binary()} | {error, atom()}.
json(Subject) ->
   json(Subject, [], []). 

-spec json(record() | proplists:proplist(), proplists:proplist()) ->
      {ok, binary()} | {error, atom()}.
json(Subject, Filters) ->
    json(Subject, Filters, []).

-spec json(record() | proplists:proplist(), proplists:proplist(),
      proplists:proplist()) -> {ok, binary()} | {error, atom()}.
json(Subject, Filters, Validators) ->
    chameleon_json:transform(Subject, Filters, Validators).
