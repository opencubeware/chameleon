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
    [{record_extraction, [sequence], [single, multiple, ets]}].

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
single(_Config) ->
    assert_unordered([name, city, country], ?RECORD_FUNCTION(company)),
    assert_unordered([name, {company, company}], ?RECORD_FUNCTION(site)),
    assert_unordered([name, surname, {site_one, site}, {site_two, site}],
                     ?RECORD_FUNCTION(person)).

multiple(_Config) ->
    assert_unordered([name, city, country], record_from_multiple(company)),
    assert_unordered([name, {company, company}], record_from_multiple(site)),
    assert_unordered([name, surname, {site_one, site}, {site_two, site}],
                     record_from_multiple(person)).

ets(_Config) ->
    assert_unordered([name, city, country], record_from_ets(company)),
    assert_unordered([name, {company, company}], record_from_ets(site)),
    assert_unordered([name, surname, {site_one, site}, {site_two, site}],
                     record_from_ets(person)).

%%%===================================================================
%%% Helpers
%%%===================================================================
assert_unordered(First, Second) ->
    [] = First--Second.

record_from_multiple(Record) ->
    {Record, Fields} = lists:keyfind(Record, 1, ?RECORD_FUNCTION()),
    Fields.

record_from_ets(Record) ->
    [{Record, Fields}] = ets:lookup(?TABLE_NAME, Record),
    Fields.
