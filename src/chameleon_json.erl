%%%===================================================================
%%% @author Rafał Studnicki <rafal@opencubeware.org>
%%% @copyright (c) 2013 opencubeware.org
%%% @doc Conversion to json from proplist/record
%%% @end
%%%===================================================================

-module(chameleon_json).

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
    throw(not_implemented);
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
