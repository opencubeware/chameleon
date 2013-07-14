%%%===================================================================
%%% @author Rafał Studnicki <rafal@opencubeware.org>
%%% @copyright (c) 2013 opencubeware.org
%%% @doc Public API
%%% @end
%%%===================================================================

-module(chameleon).

-include("chameleon.hrl").

-export([records/1]).

records(Modules) when is_list(Modules) ->
    Records = lists:foldl(fun({Module, Acc}) ->
                    case erlang:function_exported(Module,?RECORD_FUNCTION,0) of
                        true -> Acc ++ apply(Module, ?RECORD_FUNCTION, []);
                        _ -> Acc
                    end
            end, [], Modules),
    true = ets:insert(?TABLE_NAME, Records);
records(Module) ->
    records([Module]).
