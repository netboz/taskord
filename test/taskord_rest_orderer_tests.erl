-module(taskord_rest_orderer_tests).
-include_lib("eunit/include/eunit.hrl").


%% Test update_deps with no matching dependency
update_deps_no_match_test() ->
    Tasks = [{<<"task-2">>, <<"echo">>, [<<"task-3">>]}],
    Result = taskord_rest_orderer:update_deps(<<"task-1">>, Tasks, []),
    ?assertEqual([{<<"task-2">>, <<"echo">>, [<<"task-3">>]}], Result).

%% Test update_deps removes dependency
update_deps_remove_deps_test() ->
    Tasks = [{<<"task-2">>, <<"echo">>, [<<"task-1">>]}],
    Result = taskord_rest_orderer:update_deps(<<"task-1">>, Tasks, []),
    ?assertEqual([{<<"task-2">>, <<"echo">>, []}], Result).

%% Test validate_parse with missing tasks key
validate_parse_missing_tasks_test() ->
    Result = taskord_rest_orderer:validate_parse(#{foo => bar}),
    ?assertEqual({error, #{error => <<"incomplete or invalid json">>}}, Result).

%% Test validate_parse with wrong format for tasks
validate_parse_nonlist_tasks_test() ->
    Result = taskord_rest_orderer:validate_parse(#{<<"tasks">> => foo}),
    ?assertEqual({error, #{error => <<"incomplete or invalid json">>}}, Result).


%% Test validate_parse detects circular dependency
validate_parse_circular_dependency_test() ->
    Data = #{
        <<"tasks">> => [
            #{<<"name">> => <<"task-1">>, <<"command">> => <<"echo">>, <<"requires">> => [<<"task-2">>]},
            #{<<"name">> => <<"task-2">>, <<"command">> => <<"echo">>, <<"requires">> => [<<"task-3">>]},
            #{<<"name">> => <<"task-3">>, <<"command">> => <<"echo">>, <<"requires">> => [<<"task-1">>]}
        ]
    },
    Result = taskord_rest_orderer:validate_parse(Data),
    ?assertMatch({error, _}, Result).

%% Test tasks_to_bash_script
bash_script_test() ->
    Tasks = [#{<<"command">> => <<"echo hi">>}],
    Script = taskord_rest_orderer:tasks_to_bash_script(Tasks),
    ?assertMatch(<<"#!/usr/bin/env bash", _/binary>>, Script).
