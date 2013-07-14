%%%===================================================================
%%% @author Rafał Studnicki <rafal@opencubeware.org>
%%% @copyright (c) 2013 opencubeware.org
%%% @doc OTP application behaviour
%%% @end
%%%===================================================================

-module(chameleon_app).

-behaviour(application).

-include("chameleon.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ?TABLE_NAME = ets:new(?TABLE_NAME, [{read_concurrency, true},
                                        public,
                                        named_table]),
    chameleon_sup:start_link().

stop(_State) ->
    true = ets:delete(?TABLE_NAME),
    ok.
