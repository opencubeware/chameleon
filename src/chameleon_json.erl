%%%===================================================================
%%% @author Rafał Studnicki <rafal@opencubeware.org>
%%% @copyright (c) 2013 opencubeware.org
%%% @doc Conversion to json from proplist/record
%%% @end
%%%===================================================================

-module(chameleon_json).

-include("chameleon.hrl").

-export([transform/3]).

%%%===================================================================
%%% Public API
%%%===================================================================
-spec transform(record() | proplists:proplist(), proplists:proplist(),
      proplists:proplist()) -> {ok, binary()} | {error, atom()}.
% @todo handle validation, as for now - filtering only
%% from record
transform({struct, _}=Struct, _Filters, _Validators) ->
    mochijson2:encode(Struct);
transform(Record, Filters, Validators) when is_tuple(Record) ->
    RecordList = recursive_tuple_to_list(Record),
    RecordProplist = recursive_zipwith(RecordList, []),
    transform(RecordProplist, Filters, Validators);
transform([{_,_}|_]=Proplist, Filters, Validators) ->    
    Prepared = prepare_proplist(Proplist),
    transform(Prepared, Filters, Validators);
transform(Subject, _Filters, _Validators) ->
    mochijson2:encode(Subject).

%%%===================================================================
%%% Internal functions
%%%===================================================================
prepare_proplist([{_,_}|_]=Proplist) ->
    {struct, [prepare_proplist(Element) || Element <- Proplist]};
prepare_proplist({Key, [{_,_}|_]=Proplist}) ->
    {Key, prepare_proplist(Proplist)};
prepare_proplist({Key, undefined}) ->
    {Key, null};
prepare_proplist(Other) ->
    Other.

recursive_tuple_to_list(Tuple) ->
    lists:map(fun(T) when is_tuple(T) -> recursive_tuple_to_list(T);
                 (Other) -> Other
        end, tuple_to_list(Tuple)).

recursive_zipwith([], Acc) ->
    lists:reverse(Acc);
recursive_zipwith([Record | Values], Acc) ->
    [{Record, Fields}] = ets:lookup(?RECORDS_TABLE, Record),
    [{Record, recursive_values(Fields, Values, [])}|Acc].

recursive_values([], [], Acc) ->
    lists:reverse(Acc);
recursive_values([{Field, _Record}|FieldsRest], [undefined|ValuesRest], Acc) ->
    recursive_values(FieldsRest, ValuesRest, [{Field, undefined}|Acc]);
recursive_values([{Field, Record}|FieldsRest],
                 [[Record|_]=Values|ValuesRest], Acc) ->
    Acc1 = [{Field, recursive_zipwith(Values, [])}|Acc],
    recursive_values(FieldsRest, ValuesRest, Acc1);
recursive_values([Field|FieldsRest], [Value|ValuesRest], Acc) ->
    recursive_values(FieldsRest, ValuesRest, [{Field, Value}|Acc]).
