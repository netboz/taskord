
%%--------------------------------------------------------------------
%% @doc Basic REST handler for Cowboy
%%--------------------------------------------------------------------
-module(taskord_rest_orderer).
-behaviour(cowboy_rest).

-include_kernel("logger.hrl").

%% Cowboy REST callbacks
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    malformed_request/2,
    from_json/2
]).

%% Exported for testing
-export([
    validate_parse/1,
    update_deps/3,
    tasks_to_bash_script/1
]).

%%--------------------------------------------------------------------
%% @doc Initialize the handler
%%--------------------------------------------------------------------
init(Req, State) ->
    {cowboy_rest, Req, State}.

%%--------------------------------------------------------------------
%% @doc Allowed HTTP methods
%%--------------------------------------------------------------------
allowed_methods(Req, State) ->
    {[<<"POST">>, <<"OPTIONS">>], Req, State}.

%%--------------------------------------------------------------------
%% @doc Check request is correctly formatted
%%--------------------------------------------------------------------
malformed_request(Req, State) ->
    {false, Req, State}.


%%--------------------------------------------------------------------
%% @doc Content types accepted for POST
%%--------------------------------------------------------------------
content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

%%--------------------------------------------------------------------
%% @doc Handle POST requests
%%--------------------------------------------------------------------
from_json(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        #{} = Data = jsx:decode(Body),
        % Process the JSON data
        case validate_parse(Data) of
            {ok, Result} ->
                % Check if bash query parameter is present
                QsValues = cowboy_req:parse_qs(Req1),
                IsBashFormat = lists:keymember(<<"bash">>, 1, QsValues),
                %% Prepare response body and content type
                {ResponseBody, ContentType} = case IsBashFormat of
                    true ->
                        {tasks_to_bash_script(Result), <<"text/plain">>};
                    false ->
                        {jsx:encode(#{<<"tasks">> => Result}), <<"application/json">>}
                end,

                Req2 = cowboy_req:set_resp_body(ResponseBody, Req1),
                Req3 = cowboy_req:set_resp_header(<<"content-type">>, ContentType, Req2),
                {true, Req3, State};
            {error, Reason} ->
                % Return 422
                Req2 = cowboy_req:reply(
                    422,
                    #{<<"content-type">> => <<"application/json">>},
                    jsx:encode(Reason),
                    Req1),
                {stop, Req2, State}
        end
    catch
        Mod:Error ->
            % Return 400 Bad Request
            Req4 = cowboy_req:reply(
                400,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"module">> => Mod, <<"error">> => Error}),
                Req1
            ),
            {stop, Req4, State}
    end.


%%--------------------------------------------------------------------
%% @doc Validate, parse, and process the JSON data
%%--------------------------------------------------------------------
-spec validate_parse(jsx:json_term() | {'incomplete', jsx:decoder()}) -> {ok, list()} | {error, map()}.
validate_parse(#{<<"tasks">> := Tasks}) when is_list(Tasks) ->
    % First parse the tasks to tuples, then sort them,
    % putting the ones with less deps first.
    ParsedTasks = parse_tasks(Tasks, []),
    %% Then do the real processing.
    case process_tasks(ParsedTasks, []) of
        {ok, Result} ->
            {ok, Result};
        {error, Reason} ->
            {error, Reason}
    end;

validate_parse(_Data) ->
    {error, #{error => <<"incomplete or invalid json">>}}.

%%--------------------------------------------------------------------
%% @doc Parse the json tasks into a list of tuple {Name, Command, Require}
%%--------------------------------------------------------------------
-spec parse_tasks(list(), list({binary(), binary(), list(binary())})) -> list().
parse_tasks([], Acc) ->
    lists:sort(
        fun({_, _, ReqA}, {_, _, ReqB}) when is_list(ReqA), is_list(ReqB) ->
            length(ReqA) =< length(ReqB)
        end,
        Acc
    );
parse_tasks([#{<<"name">> := Name, <<"command">> := Command} = Task | OtherTasks], Acc) ->
    parse_tasks(OtherTasks, [{Name, Command, maps:get(<<"requires">>, Task, [])} | Acc]).


%%--------------------------------------------------------------------
%% @doc This actually process the tasks and return the ordered list
%% This is done by First processing the tasks with no deps, then updating
%% the deps of the remaining tasks. This recursively until there is no
%% more tasks.
%%--------------------------------------------------------------------
-spec process_tasks(
    list({binary(), binary(), list(binary())}),
    list()
) ->
    {ok, list()} | {error, map()}.

process_tasks([], Acc) ->
    {ok, lists:reverse(Acc)};
process_tasks([{Name, Command, []} | Rest], Acc) ->
    %% No deps we can process this one
    UpdatedRest = update_deps(Name, Rest, []),
    TaskMap = #{<<"name">> => Name, <<"command">> => Command},
    process_tasks(UpdatedRest, [TaskMap | Acc]);
%% Tasks were sorted upon length of requires list, at the end of parse tasks.
%% It means tasks ready to process should be at the beginning of the list.
%% If there isn't, it means we have a missing dep or cycle
process_tasks([{Name, _Command, Require} | _Rest], _Acc) when Require =/= [] ->
    logger:warning("Missing deps or cycle: ~p", [{Name, Require}]),
    {error, #{error => missing_deps_or_cycle, task_name => Name, requires => Require}}.

%%--------------------------------------------------------------------
%% @doc Update the dependencies by removing the validated dep from the
%% require list.
%% Ready tasks ( no deps ) are separaed from the other ones (with deps) to
%% be concatenated once at the end (instead of doing it for each task).
%%--------------------------------------------------------------------
-spec update_deps(binary(), list({binary(), binary(), list(binary())}), list()) -> list().

update_deps(ValidatedDepName, Tasks, Acc) ->
    update_deps(ValidatedDepName, Tasks, [], Acc).

update_deps(_ValidatedDepName, [], ReadyTasks, StillWaiting) ->
    ReadyTasks ++ StillWaiting;
update_deps(ValidatedDepName, [{Name, Command, Require} | Rest], ReadyTasks, StillWaiting) ->
    case lists:delete(ValidatedDepName, Require) of
        [] ->
            % Task is now ready - add to front of ready tasks
            update_deps(ValidatedDepName, Rest, [{Name, Command, []} | ReadyTasks], StillWaiting);
        NewRequire ->
            % Task still has dependencies - add to front of waiting tasks
            update_deps(ValidatedDepName, Rest, ReadyTasks, [{Name, Command, NewRequire} | StillWaiting])
    end.

%%--------------------------------------------------------------------
%% @doc Convert ordered tasks to bash script
%%--------------------------------------------------------------------
-spec tasks_to_bash_script(list(map())) -> binary().
tasks_to_bash_script(Tasks) ->
    Shebang = <<"#!/usr/bin/env bash\n">>,
    Commands = lists:map(fun(#{} =Task) ->
        Command = maps:get(<<"command">>, Task),
        <<Command/binary, "\n">>
    end, Tasks),
    iolist_to_binary([Shebang | Commands]).
