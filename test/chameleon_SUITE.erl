%%%===================================================================
%%% @author Rafał Studnicki <rafal@opencubeware.org>
%%% @copyright (c) 2013 opencubeware.org
%%% @doc Chameleon Common Test suite
%%% @end
%%%===================================================================

-module(chameleon_SUITE).
-compile({parse_transform, chameleon_transform}).
-compile(export_all).

-include("chameleon.hrl").
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Extracted records definitions
%%%===================================================================
-record(company, {name, city :: list(), country :: integer()}).
-record(site, {name, company :: #company{}}).
-record(person, {name, surname,
                 site_one :: #site{},
                 site_two :: #site{}}).

%%%===================================================================
%%% Suite configuration
%%%===================================================================
suite() ->
    [].

all() ->
    [{group, record_extraction}].

groups() ->
    [{record_extraction, [sequence], [multiple, ets]}].

%%%===================================================================
%%% Init and teardown
%%%===================================================================
init_per_suite(Config) ->
    ok = application:start(chameleon),
    chameleon:records(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestcaseName, Config) ->
    Config.

end_per_testcase(_TestcaseName, _Config) ->
    ok.

%%%===================================================================
%%% Tests
%%%===================================================================
multiple(_Config) ->
    [name, city, country] = record_from_multiple(company),
    [name, {company, company}] = record_from_multiple(site),
    [name, surname, {site_one, site}, {site_two, site}] = record_from_multiple(person).

ets(_Config) ->
    [name, city, country] = record_from_ets(company),
    [name, {company, company}] = record_from_ets(site),
    [name, surname, {site_one, site}, {site_two, site}] = record_from_ets(person).

%%%===================================================================
%%% Helpers
%%%===================================================================
record_from_multiple(Record) ->
    {Record, Fields} = lists:keyfind(Record, 1, ?RECORD_FUNCTION()),
    Fields.

record_from_ets(Record) ->
    [{Record, Fields}] = ets:lookup(?RECORDS_TABLE, Record),
    Fields.
