%%%===================================================================
%%% @author Rafał Studnicki <rafal@opencubeware.org>
%%% @copyright (c) 2013 opencubeware.org
%%% @doc Chameleon E-Unit tests
%%% @end
%%%===================================================================

-module(chameleon_eunit).
-compile({parse_transform, chameleon_transform}).

-include("chameleon.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(FIELDS(X), record_fields(X)).

-record(company, {name, city :: list(), country :: integer()}).
-record(site, {name, company :: #company{}}).
-record(person, {name, surname,
                 site_one :: #site{},
                 site_two :: #site{}}).

company_test() ->
    %% because we don't care about the order of attributes
    Desired = [name,city,country],
    [] = ?FIELDS(company)--Desired.

site_test() ->
    Desired = [name,{company,{company,?FIELDS(company)}}],
    [] = ?FIELDS(site)--Desired.

person_test() ->
    Desired = [name,surname,
               {site_one,{site,?FIELDS(site)}},
               {site_two,{site,?FIELDS(site)}}],
    [] = ?FIELDS(person)--Desired.

record_fields(Record) ->
    {Record, Fields} = lists:keyfind(Record, 1, ?RECORD_FUNCTION()),
    Fields.
