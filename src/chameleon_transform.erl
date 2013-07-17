%%%===================================================================
%%% @author Rafał Studnicki <rafal@opencubeware.org>
%%% @copyright (c) 2013 opencubeware.org
%%% @doc Parse transform for records information extraction
%%% @end
%%%===================================================================

-module(chameleon_transform).
-export([parse_transform/2]).

-include("chameleon.hrl").

-record(state, {records=[], exported=false}).

parse_transform(Input, _) ->
    State1 = extract_records(Input, #state{}),
    State2 = extract_types(Input, State1),
    modify_tree(Input, [], State2).

modify_tree([{eof,_}=Eof], Acc, #state{records=Records}) ->
    RecordsCons = lists:foldl(fun(Record, Acc1) ->
                    {cons, 0, generate_record(Record), Acc1}
            end, {nil, 0}, Records),
    Function0 = {function, 0, ?RECORD_FUNCTION, 0,
                 [{clause, 0, [], [], [RecordsCons]}]},
    lists:reverse([Eof, Function0 | Acc]);
modify_tree([{attribute, _, record, _}|_]=Tree, Acc,
            #state{exported=false}=State) ->
    Acc1 = [{attribute, 0, export, [{?RECORD_FUNCTION,0}]}|Acc],
    modify_tree(Tree, Acc1, State#state{exported=true});
modify_tree([Element|Rest], Acc, State) ->
    modify_tree(Rest, [Element|Acc], State).

generate_record({Name, Fields}) ->
    {tuple, 0, [{atom, 0, Name}, abstract_fields(Fields)]}.

abstract_fields(Fields) ->
    lists:foldr(fun(Field, Acc) ->
                {cons, 0, abstract_field(Field), Acc}
        end, {nil, 0}, Fields).

abstract_field({Field, Record}) ->
    {tuple, 0, [{atom, 0, Field},
                {atom, 0, Record}]};
abstract_field(Field) ->
    {atom, 0, Field}.

extract_types([{attribute, _, type, {{record, Record}, AbsFields, _}}|Rest],
              #state{records=Records}) ->
    Records1 = lists:foldl(fun({typed_record_field,
                                {record_field, _, {atom, _, Field}},
                                {type, _, union, [{atom, _, undefined},
                                                  {type, _, record,
                                                   [{atom, _, Relation}]}]}},
                               Acc) ->
                    case lists:keyfind(Relation, 1, Acc) of
                        {Relation, _} ->
                            {Record, Fields} = lists:keyfind(Record, 1, Acc),
                            Fields1 = lists:map(fun(FieldMaybe) ->
                                            case FieldMaybe of
                                                    Field -> {Field, Relation};
                                                    Other -> Other
                                            end
                                    end, Fields),
                            lists:keystore(Record, 1, Acc, {Record, Fields1});
                        _ ->
                            Acc
                    end;
                (_, Acc) -> Acc end, Records, AbsFields),
    extract_types(Rest, #state{records=Records1});
extract_types([_Other|Rest], State) ->
    extract_types(Rest, State);
extract_types([], State) ->
    State.

extract_records([{attribute, _, record, {Name, AbsFields}}|Rest],
                #state{records=Records}) ->
    Fields = [Field || {record_field, _, {atom, _, Field}} <- AbsFields],
    extract_records(Rest, #state{records = [{Name, Fields}|Records]});
extract_records([_Other|Rest], State) ->
    extract_records(Rest, State);
extract_records([], State) ->
    State.
