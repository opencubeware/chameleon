%%%===================================================================
%%% @author Rafał Studnicki <rafal@opencubeware.org>
%%% @copyright (c) 2013 opencubeware.org
%%% @doc Record extraction parse_transform tests
%%% @end
%%%===================================================================

-module(chameleon_record_test).
-compile({parse_transform, chameleon_transform}).

-include_lib("eunit/include/eunit.hrl").

-record(company, {name, city :: list(), country :: integer()}).
-record(site, {name, company :: #company{}}).
-record(person, {name, surname,
                 site_one :: #site{},
                 site_two :: #site{}}).

company_test() ->
    %% because we don't care about the order of attributes
    Desired = [name,city,country],
    [] = chameleon_record(company)--Desired.

site_test() ->
    Desired = [name,{company,{company,chameleon_record(company)}}],
    [] = chameleon_record(site)--Desired.

person_test() ->
    Desired = [name,surname,
               {site_one,{site,chameleon_record(site)}},
               {site_two,{site,chameleon_record(site)}}],
    [] = chameleon_record(person)--Desired.
