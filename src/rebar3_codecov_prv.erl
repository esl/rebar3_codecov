-module(rebar3_codecov_prv).
-behaviour(provider).

-include("include/rebar3_codecov_logger.hrl").

%% API
-export([init/1, do/1, format_error/1]).

-define(NAMESPACE, codecov).
-define(PROVIDER, analyze).
-define(DEPS, [app_discovery]).
-define(DESC, "Parce .coverdata files to JSON").

%% Public API
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                 {name, ?PROVIDER},            % The 'user friendly' name of the task
                                 {namespace, ?NAMESPACE},
                                 {module, ?MODULE},            % The module implementation of the task
                                 {bare, true},                 % The task can be run by the user, always true
                                 {deps, ?DEPS},                % The list of dependencies
                                 {example, "rebar3 rebar3_codecov"}, % How to use the plugin
                                 {opts, []},                   % list of options understood by the plugin
                                 {short_desc, ?DESC},
                                 {desc, ?DESC}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("~nExporting cover data ...~n", []),
    BuildDirectory = rebar_dir:profile_dir(State),
    CoverDirectory = filename:join([BuildDirectory, "cover/*.coverdata"]),
    [InFile] = filelib:wildcard(CoverDirectory),
    analyze([InFile, "codecov.json"]),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Private API

to_json(OutputFile) ->
    Modules = cover:imported_modules(),
    {result, Result, _} = cover:analyse(Modules, calls, line),
    Mod2Data = lists:foldl(fun add_cover_line_into_array/2, #{}, Result),
    JSON = maps:fold(fun format_array_to_list/3, [], Mod2Data),
    Binary = jiffy:encode(#{<<"coverage">> => {JSON}}),
    file:write_file(OutputFile, Binary).

add_cover_line_into_array({{Module, Line}, CallTimes}, Acc) ->
    CallsPerLineArray = maps:get(Module, Acc, array:new({default, null})),
    Acc#{Module => array:set(Line, CallTimes, CallsPerLineArray)}.

format_array_to_list(Module, CallsPerLineArray, Acc) ->
    ListOfCallTimes = array:to_list(CallsPerLineArray),
    BinPath = list_to_binary(get_source_path(Module)),
    [{BinPath, ListOfCallTimes}|Acc].

get_source_path(Module) when is_atom(Module) ->
    try
        AbsPath = proplists:get_value(source, Module:module_info(compile)),
        [Prefix, Suffix] = string:split(AbsPath, "src/"),
        filename:join("/src", Suffix)
    catch Error:Reason ->
              Issue = io_lib:format("get_source_path/1 failed, module=~p", [Module]),
              rebar_api:warn("~p~n~p~n~p~n~p~n", [Issue, Error, Reason, erlang:get_stacktrace()]),
              atom_to_list(Module) ++ ".erl"
    end.

analyze(Args) ->
    try
        [InFile, OutFile] = Args,
        cover:start(),
        rebar_api:info("importing ~s~n", [InFile]),
        ok = cover:import(InFile),
        rebar_api:info("exporting ~s~n", [OutFile]),
        to_json(OutFile)
    catch Error:Reason ->
              rebar_api:abort("~p~n~p~n~p~n",[Error, Reason, erlang:get_stacktrace()])
    end.
