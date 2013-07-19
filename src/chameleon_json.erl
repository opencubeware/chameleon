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
transform(Subject, Filters, Validators) ->
    Records = dict:from_list(ets:tab2list(?RECORDS_TABLE)),
    Prepared = prepare(Subject, Records),
    mochijson2:encode(Prepared).

%%%===================================================================
%%% Internal functions
%%%===================================================================
prepare({Key, [{_,_}|_]=Proplist}, Records) ->
    {Key, prepare(Proplist, Records)};
prepare({Key, undefined}, _Records) ->
    {Key, null};
prepare(Record, Records) when is_tuple(Record) ->
    RecordList = recursive_tuple_to_list(Record),
    case dict:is_key(hd(RecordList), Records) of
        true ->
            RecordProplist = recursive_zipwith(RecordList, [], Records),
            prepare(RecordProplist, Records);
        false ->
            Record
    end;
prepare([{_,_}|_]=Proplist, Records) ->
    {struct, [prepare(Element, Records) || Element <- Proplist]};
prepare(List, Records) when is_list(List) ->
    [prepare(Element, Records) || Element <- List];
prepare(Other, _Records) ->
    Other.

recursive_tuple_to_list(Tuple) ->
    lists:map(fun(T) when is_tuple(T) -> recursive_tuple_to_list(T);
                 (Other) -> Other
        end, tuple_to_list(Tuple)).

recursive_zipwith([], Acc, _Records) ->
    lists:reverse(Acc);
recursive_zipwith([Record | Values], Acc, Records) ->
    Fields = dict:fetch(Record, Records),
    [{Record, recursive_values(Fields, Values, [], Records)}|Acc].

recursive_values([], [], Acc, _Records) ->
    lists:reverse(Acc);
recursive_values([{Field, _Record}|FieldsRest], [undefined|ValuesRest],
                 Acc, Records) ->
    recursive_values(FieldsRest, ValuesRest, [{Field, undefined}|Acc], Records);
recursive_values([{Field, Record}|FieldsRest],
                 [[Record|_]=Values|ValuesRest], Acc, Records) ->
    Acc1 = [{Field, recursive_zipwith(Values, [], Records)}|Acc],
    recursive_values(FieldsRest, ValuesRest, Acc1, Records);
recursive_values([Field|FieldsRest], [Value|ValuesRest], Acc, Records) ->
    recursive_values(FieldsRest, ValuesRest, [{Field, Value}|Acc], Records).
