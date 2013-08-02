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
    Tree = modify_tree(Input, [], State2),
    Tree.

modify_tree([{eof,_}=Eof], Acc, #state{records=Records}) ->
    RecordsCons = erl_parse:abstract(Records),
    Function0 = {function, 0, ?RECORD_FUNCTION, 0,
                 [{clause, 0, [], [], [RecordsCons]}]},
    lists:reverse([Eof, Function0 | Acc]);
modify_tree([{attribute, _, record, _}|_]=Tree, Acc,
            #state{exported=false}=State) ->
    Acc1 = [{attribute, 0, export, [{?RECORD_FUNCTION,0}]}|Acc],
    modify_tree(Tree, Acc1, State#state{exported=true});
modify_tree([Element|Rest], Acc, State) ->
    modify_tree(Rest, [Element|Acc], State).

extract_types([{attribute, _, type, {{record, Record}, FieldsAbs, _}}|Rest],
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
                            Fields1 = lists:map(fun({FieldMaybe, Default}) ->
                                            case FieldMaybe of
                                                    Field -> {Field, Relation, Default};
                                                    Other -> {Other, Default}
                                            end;
                                        (Other) -> Other
                                    end, Fields),
                            lists:keystore(Record, 1, Acc, {Record, Fields1});
                        _ ->
                            Acc
                    end;
                (_, Acc) -> Acc end, Records, FieldsAbs),
    extract_types(Rest, #state{records=Records1});
extract_types([_Other|Rest], State) ->
    extract_types(Rest, State);
extract_types([], State) ->
    State.

extract_records([{attribute, _, record, {Name, AbsFields}}|Rest],
                #state{records=Records}) ->
    Fields = lists:map(fun({record_field, _, {atom, _, Field}}) ->
                    {Field, undefined};
                ({record_field, _, {atom, _, Field}, DefaultAbs}) ->
                    {Field, erl_parse:normalise(DefaultAbs)}
            end, AbsFields),
    extract_records(Rest, #state{records = [{Name, Fields}|Records]});
extract_records([_Other|Rest], State) ->
    extract_records(Rest, State);
extract_records([], State) ->
    State.
