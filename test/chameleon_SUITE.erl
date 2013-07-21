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
    [{group, record_extraction},
     {group, json},
     {group, proplist},
     {group, record}].

groups() ->
    [{record_extraction, [sequence], [multiple,
                                      ets]},
     {json, [sequence], [json_simple_terms,
                         json_proplist,
                         json_nested_proplist,
                         json_proplist_list,
                         json_record,
                         json_record_nested,
                         json_record_list,
                         json_record_proplist]},
     {proplist, [sequence], [proplist,
                             proplist_negative,
                             proplist_list]},
     {record, [sequence], [record,
                           record_negative,
                           record_list]}
    ].

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

json_simple_terms(_Config) ->
    assert_json(<<"\"dummy\"">>, dummy),
    assert_json(<<"[1,2,3]">>, [1,2,3]),
    assert_json(<<"[\"one\",\"two\",3]">>, [one, <<"two">>, 3]). 

json_proplist(_Config) ->
    Json = <<"{\"name\":\"Alice\",\"surname\":\"Doe\",\"city\":\"London\","
             "\"country\":\"England\",\"phone\":568111,\"pet\":null}">>,
    Proplist = [{name, <<"Alice">>}, {surname, <<"Doe">>}, {city, <<"London">>},
                {country, <<"England">>}, {phone, 568111}, {pet, undefined}],
    assert_json(Json, Proplist).

json_nested_proplist(_Config) ->
    Json = <<"{\"name\":\"Alice\",\"surname\":\"Doe\","
             "\"address\":{\"city\":\"London\","
             "\"country\":\"England\",\"street\":"
             "{\"name\":\"Oxford St\",\"number\":12}}}">>,
    Proplist = [{name, <<"Alice">>}, {surname, <<"Doe">>},
                {address, [{city, <<"London">>},
                           {country, <<"England">>},
                           {street, [{name, <<"Oxford St">>},
                                     {number, 12}]}]}],
    assert_json(Json, Proplist).

json_proplist_list(_Config) ->
    Json = <<"[{\"name\":\"Alice\",\"city\":\"London\"},"
              "{\"name\":\"Bob\",\"city\":\"New York\"}]">>,
    List = [[{name, <<"Alice">>}, {city, <<"London">>}],
            [{name, <<"Bob">>}, {city, <<"New York">>}]],
    assert_json(Json, List).

json_record(_Config) ->
    Json = <<"{\"company\":"
             "{\"name\":\"ACME\","
             "\"city\":\"London\","
             "\"country\":\"England\"}}">>,
    Record = #company{name = <<"ACME">>,
                      city = <<"London">>,
                      country = <<"England">>},
    assert_json(Json, Record).

json_record_nested(_Config) ->
    Json1 = <<"{\"site\":"
              "{\"name\":\"Secret Department\","
              "\"company\":null}}">>,
    Record1 = #site{name = <<"Secret Department">>},
    assert_json(Json1, Record1),

    JsonCompany = <<"{\"company\":"
                    "{\"name\":\"ACME\","
                    "\"city\":\"London\","
                    "\"country\":\"England\"}}">>,
    Json2 = <<"{\"site\":"
              "{\"name\":\"Secret Department\","
              "\"company\":",JsonCompany/binary,"}}">>,
    Record2 = #site{name = <<"Secret Department">>,
                    company = #company{name = <<"ACME">>,
                                       city = <<"London">>,
                                       country = <<"England">>}},
    assert_json(Json2, Record2).

json_record_list(_Config) ->
    Json1 = <<"{\"site\":"
              "{\"name\":\"Secret Department\","
              "\"company\":null}}">>,
    Json2 = <<"{\"company\":"
              "{\"name\":\"ACME\","
              "\"city\":\"London\","
              "\"country\":\"England\"}}">>,
    Json = <<"[", Json1/binary, ",", Json2/binary, "]">>,
    List = [#site{name = <<"Secret Department">>},
            #company{name = <<"ACME">>,
                     city = <<"London">>,
                     country = <<"England">>}],
    assert_json(Json, List).

json_record_proplist(_Config) ->
    Json1 = <<"{\"site\":"
              "{\"name\":\"Secret Department\","
              "\"company\":null}}">>,
    Json2 = <<"[{\"name\":\"Alice\",\"city\":\"London\"},"
              "{\"name\":\"Bob\",\"city\":\"New York\"}]">>,
    Json3 = <<"{\"name\":\"Alice\",\"surname\":\"Doe\","
              "\"address\":{\"city\":\"London\","
              "\"country\":\"England\",\"street\":"
              "{\"name\":\"Oxford St\",\"number\":12}}}">>,
    Json = <<"[", Json1/binary, ",", Json2/binary, ",", Json3/binary, "]">>,
    List = [#site{name = <<"Secret Department">>},
            [[{name, <<"Alice">>}, {city, <<"London">>}],
             [{name, <<"Bob">>}, {city, <<"New York">>}]],
            [{name, <<"Alice">>}, {surname, <<"Doe">>},
             {address, [{city, <<"London">>},
                        {country, <<"England">>},
                        {street, [{name, <<"Oxford St">>},
                                  {number, 12}]}]}]],
    assert_json(Json, List).

proplist(_Config) ->
    Json = <<"{\"name\":\"Alice\",\"surname\":\"Doe\","
             "\"address\":{\"city\":\"London\","
             "\"country\":\"England\",\"street\":"
             "{\"name\":\"Oxford St\",\"number\":12}}}">>,
    Proplist = [{<<"name">>, <<"Alice">>},
                {<<"surname">>, <<"Doe">>},
                {<<"address">>, [{<<"city">>, <<"London">>},
                                 {<<"country">>, <<"England">>},
                                 {<<"street">>, [{<<"name">>, <<"Oxford St">>},
                                                 {<<"number">>, 12}]}]}],
    {ok, Proplist} = chameleon:proplist(Json).

proplist_negative(_Config) ->
    Json = <<"{\"name\":\"Alice\",\"surname\":\"Doe\","
             "\"address\":{\"city\":\"London\","
             "\"country\":\"England\",\"street\":"
             "{\"name\":\"Oxford St\",\"number\":1}">>,
    {error, unprocessable} = chameleon:proplist(Json).

proplist_list(_Config) ->
    Json = <<"[{\"name\":\"Alice\",\"city\":\"London\"},"
              "{\"name\":\"Bob\",\"city\":\"New York\"}]">>,
    List = [[{<<"name">>, <<"Alice">>},
             {<<"city">>, <<"London">>}],
            [{<<"name">>, <<"Bob">>},
             {<<"city">>, <<"New York">>}]],
    {ok, List} = chameleon:proplist(Json).

record(_Config) ->
    Json1 = <<"{\"site\":"
              "{\"name\":\"Secret Department\","
              "\"company\":null}}">>,
    Record1 = #site{name = <<"Secret Department">>},
    {ok, Record1} = chameleon:record(Json1),

    JsonCompany = <<"{\"company\":"
                    "{\"name\":\"ACME\","
                    "\"city\":\"London\","
                    "\"country\":\"England\"}}">>,
    Json2 = <<"{\"site\":"
              "{\"name\":\"Secret Department\","
              "\"company\":",JsonCompany/binary,"}}">>,
    Record2 = #site{name = <<"Secret Department">>,
                    company = #company{name = <<"ACME">>,
                                       city = <<"London">>,
                                       country = <<"England">>}},
    {ok, Record2} = chameleon:record(Json2).

record_negative(_Config) ->
    Json1 = <<"{\"site\":"
              "\"name\":\"Secret Department\","
              "\"company\":null}}">>,
    {error, unprocessable} = chameleon:record(Json1),

    Json2 = <<"{\"inexistent\":"
              "\"field_one\": 1,"
              "\"field_two\": \"some_value\"}">>,
    {error, unprocessable} = chameleon:record(Json2).

record_list(_Config) ->
    Json1 = <<"{\"site\":"
              "{\"name\":\"Secret Department\","
              "\"company\":null}}">>,
    Record1 = #site{name = <<"Secret Department">>},
    JsonCompany = <<"{\"company\":"
                    "{\"name\":\"ACME\","
                    "\"city\":\"London\","
                    "\"country\":\"England\"}}">>,
    Company = #company{name = <<"ACME">>,
                       city = <<"London">>,
                       country = <<"England">>},
    Json2 = <<"{\"site\":"
              "{\"name\":\"Secret Department\","
              "\"company\":",JsonCompany/binary,"}}">>,
    Record2 = #site{name = <<"Secret Department">>,
                    company = Company},
    Json = <<"[", Json1/binary, ",", Json2/binary, ",", JsonCompany/binary, "]">>,
    {ok, [Record1, Record2, Company]} = chameleon:record(Json).

%%%===================================================================
%%% Helpers
%%%===================================================================
assert_json(Json, Term) ->
    {ok, JsonFromTerm} = chameleon:json(Term),
    Json = iolist_to_binary(JsonFromTerm).

record_from_multiple(Record) ->
    {Record, Fields} = lists:keyfind(Record, 1, ?RECORD_FUNCTION()),
    Fields.

record_from_ets(Record) ->
    [{Record, Fields}] = ets:lookup(?RECORDS_TABLE, Record),
    Fields.
