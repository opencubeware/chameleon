%%%===================================================================
%%% @author Rafał Studnicki <rafal@opencubeware.org>
%%% @copyright (c) 2013 opencubeware.org
%%% @doc Conversion to json from proplist/record
%%% @end
%%%===================================================================

-module(chameleon_json).

-include("chameleon.hrl").

-export([transform/2]).

%%%===================================================================
%%% Public API
%%%===================================================================
-spec transform(record() | proplists:proplist() | binary(),
                chameleon:filter()) -> 
    {ok, binary()} | {ok, record()} |
    {ok, proplists:proplist()} | {error, chameleon:error()}.
transform(Subject, Filter) ->
    try
        do_transform(Subject, Filter)
    catch _:_ ->
        {error, unprocessable}
    end.

do_transform({record, [{_,_}|_]=Proplist}, Filter) ->
    Record = proplist_to_record(Proplist, Filter),
    {ok, Record};
do_transform({record, Binary}, Filter) ->
    Proplist = binary_to_proplist(Binary), 
    Record = proplist_to_record(Proplist, Filter), 
    {ok, Record};
do_transform({proplist, Record}, _Filter)
        when is_tuple(Record); is_list(Record) ->
    Records = dict:from_list(ets:tab2list(?RECORDS_TABLE)),
    Proplist = record_to_proplist(Record, Records),
    {ok, Proplist};
do_transform({proplist, Binary}, _Filter) when is_binary(Binary) ->
    Proplist = binary_to_proplist(Binary),
    {ok, Proplist};
do_transform({json, Subject}, Filter) ->
    Records = dict:from_list(ets:tab2list(?RECORDS_TABLE)),
    Prepared = prepare_struct(Subject, Records, Filter),
    {ok, mochijson2:encode(Prepared)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
prepare_struct({Key, [{_,_}|_]=Proplist}, Records, Filter) ->
    {Key, prepare_struct(Proplist, Records, Filter)};
prepare_struct({Key, undefined}, _Records, _Filter) ->
    {Key, null};
prepare_struct(Record, Records, Filter) when is_tuple(Record) ->
    [RecordName|_] = tuple_to_list(Record),
    Filter2 = case ets:lookup(?OUTPUT_FILTERS_TABLE, RecordName) of
        [{RecordName, Filter1}] -> joint_filter(Filter1, Filter);
        _                       -> Filter
    end,
    FilteredRecord = Filter2(Record),
    case dict:is_key(RecordName, Records) of
        true ->
            RecordList = recursive_tuple_to_list(FilteredRecord),
            RecordProplist = recursive_zip(RecordList, [], Records, yes),
            prepare_struct(RecordProplist, Records, fun(X) -> X end);
        false ->
            Record
    end;
prepare_struct([{_,_}|_]=Proplist, Records, Filter) ->
    {struct, [prepare_struct(Element, Records, Filter) || Element <- Proplist]};
prepare_struct(List, Records, Filter) when is_list(List) ->
    [prepare_struct(Element, Records, Filter) || Element <- List];
prepare_struct(Other, _Records, _Filter) ->
    Other.

recursive_tuple_to_list(Tuple) ->
    lists:map(fun(T) when is_tuple(T) -> recursive_tuple_to_list(T);
                 (Other) -> Other
        end, tuple_to_list(Tuple)).

recursive_zip([], Acc, _Records, _Roots) ->
    lists:reverse(Acc);
recursive_zip([Record | Values], Acc, Records, Roots) ->
    Fields = dict:fetch(Record, Records),
    RecVal = recursive_values(Fields, Values, [], Records, Roots),
    case Roots of
        yes -> [{Record, RecVal} | Acc];
        no  -> RecVal ++ Acc
    end.

recursive_values([], [], Acc, _Records, _Roots) ->
    lists:reverse(Acc);
recursive_values([{Field,_Record,_Default}|FieldsRest], [undefined|ValuesRest],
                 Acc, Records, Roots) ->
    recursive_values(FieldsRest, ValuesRest, [{Field, undefined}|Acc],
                     Records, Roots);
recursive_values([{Field,Record,_Default}|FieldsRest],
                 [[Record|_]=Values|ValuesRest], Acc, Records, Roots) ->
    Acc1 = [{Field, recursive_zip(Values, [], Records, Roots)}|Acc],
    recursive_values(FieldsRest, ValuesRest, Acc1, Records, Roots);
recursive_values([{Field,_Default}|FieldsRest], [Value|ValuesRest], Acc,
                 Records, Roots) ->
    recursive_values(FieldsRest, ValuesRest, [{Field, Value}|Acc],
                     Records, Roots).

binary_to_proplist(Binary) ->
    Struct = mochijson2:decode(Binary),
    struct_to_proplist(Struct).

struct_to_proplist(List) when is_list(List) ->
    [struct_to_proplist(Element) || Element <- List];
struct_to_proplist({struct, Proplist}) ->
    [{Key, struct_to_proplist(Value)} || {Key, Value} <- Proplist];
struct_to_proplist(Other) ->
    Other.

proplist_to_record([{Record, Proplist}], Filter) when is_atom(Record) ->
    proplist_to_record1(Record, Proplist, Filter, fun find_field/4);
proplist_to_record([{RecordB, Proplist}], Filter) ->
    Record = binary_to_existing_atom(RecordB, utf8),
    proplist_to_record1(Record, Proplist, Filter, fun find_field_binary/4);
proplist_to_record(List, Filter) when is_list(List) ->
    [proplist_to_record(Record, Filter) || Record <- List].

proplist_to_record1(Record, Proplist, Filter, FindField) ->
    Dict = dict:from_list(Proplist),
    [{Record, Fields}] = ets:lookup(?RECORDS_TABLE, Record),
    Values = lists:map(fun({Field, _Relation, Default}) ->
                    FindField(Field, Dict, fun proplist_to_record/2,
                               Default);
                ({Field, Default}) ->
                    FindField(Field, Dict, fun(X,_F) -> X end,
                               Default)
            end, Fields),
    Filter2 = case ets:lookup(?INPUT_FILTERS_TABLE, Record) of
        [{Record, Filter1}] -> joint_filter(Filter1, Filter);
        _                   -> Filter
    end,
    Filter2(list_to_tuple([Record | Values])).

find_field(Field, Dict, Map, Default) ->
    case dict:find(Field, Dict) of
        {ok, null} -> Default;
        {ok, Value} -> Map(Value, fun(X) -> X end);
        error -> Default
    end.

find_field_binary(Field, Dict, Map, Default) ->
    FieldB = atom_to_binary(Field, utf8),
    find_field(FieldB, Dict, Map, Default).

joint_filter(F1, F2) ->
    fun(X) -> F2(F1(X)) end.

record_to_proplist(List, Records) when is_list(List) ->
    [record_to_proplist(Record, Records) || Record <- List];
record_to_proplist(Record, Records) ->
    RecordList = recursive_tuple_to_list(Record),
    recursive_zip(RecordList, [], Records, no).
