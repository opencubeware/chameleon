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
-spec transform(record() | proplists:proplist() | binary(),
                 [chameleon:filter()], [chameleon:validator()]) -> 
    {ok, binary()} | {ok, record()} |
    {ok, proplists:proplist()} | {error, atom()}.
% @todo handle validation, as for now - filtering only
transform({record, Binary}, Filters, Validators) ->
    Proplist = binary_to_proplist(Binary),
    {ok, proplist_to_record(Proplist)};
transform({proplist, Binary}, Filters, Validators) ->
    {ok, binary_to_proplist(Binary)};
transform({json, Subject}, Filters, Validators) ->
    Records = dict:from_list(ets:tab2list(?RECORDS_TABLE)),
    Prepared = prepare_struct(Subject, Records),
    {ok, mochijson2:encode(Prepared)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
prepare_struct({Key, [{_,_}|_]=Proplist}, Records) ->
    {Key, prepare_struct(Proplist, Records)};
prepare_struct({Key, undefined}, _Records) ->
    {Key, null};
prepare_struct(Record, Records) when is_tuple(Record) ->
    RecordList = recursive_tuple_to_list(Record),
    case dict:is_key(hd(RecordList), Records) of
        true ->
            RecordProplist = recursive_zipwith(RecordList, [], Records),
            prepare_struct(RecordProplist, Records);
        false ->
            Record
    end;
prepare_struct([{_,_}|_]=Proplist, Records) ->
    {struct, [prepare_struct(Element, Records) || Element <- Proplist]};
prepare_struct(List, Records) when is_list(List) ->
    [prepare_struct(Element, Records) || Element <- List];
prepare_struct(Other, _Records) ->
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

binary_to_proplist(Binary) ->
    Struct = mochijson2:decode(Binary),
    struct_to_proplist(Struct).

struct_to_proplist(List) when is_list(List) ->
    [struct_to_proplist(Element) || Element <- List];
struct_to_proplist({struct, Proplist}) ->
    [{Key, struct_to_proplist(Value)} || {Key, Value} <- Proplist];
struct_to_proplist(Other) ->
    Other.

proplist_to_record([{RecordB, Proplist}]) ->
    Dict = dict:from_list(Proplist),
    Record = binary_to_existing_atom(RecordB, utf8),
    [{Record, Fields}] = ets:lookup(?RECORDS_TABLE, Record),
    Values = lists:map(fun({Field, _Relation}) ->
                    find_field(Field, Dict, fun proplist_to_record/1);
                (Field) ->
                    find_field(Field, Dict, fun(X) -> X end)
            end, Fields),
    list_to_tuple([Record | Values]);
proplist_to_record(List) when is_list(List) ->
    [proplist_to_record(Record) || Record <- List].

find_field(Field, Dict, Map) ->
    FieldB = atom_to_binary(Field, utf8),
    case dict:find(FieldB, Dict) of
        {ok, null} -> undefined;
        {ok, Value} -> Map(Value);
        error -> undefined
    end.
