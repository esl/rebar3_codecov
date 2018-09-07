-module(rebar3_codecov_prv).
-behaviour(provider).

%% API
-export([init/1, do/1, format_error/1]).

-define(NAMESPACE, codecov).
-define(PROVIDER, analyze).
-define(DEPS, [{default, app_discovery}]).
-define(DESC, "Converts .coverdata files to codecov compatible JSON").
-define(OUT_FILE, "codecov.json").

%% Public API
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                 {name, ?PROVIDER},
                                 {namespace, ?NAMESPACE},
                                 {module, ?MODULE},
                                 {bare, true},
                                 {deps, ?DEPS},
                                 {example, "rebar3 rebar3_codecov"},
                                 {opts, []},
                                 {short_desc, ?DESC},
                                 {desc, ?DESC}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    Apps = filter_apps(RawOpts, State),
    lists:map(fun(App) ->  add_profile_ebin_path(App, State) end, Apps),
    AppsInfo = rebar_state:project_apps(State),
    Defaults = lists:map(fun(App) -> EBin = rebar_app_info:ebin_dir(App),
                                     rebar_dir:make_relative_path(EBin, rebar_dir:root_dir(State)) end, AppsInfo),
    code:add_paths(Defaults),
    rebar_api:info("~nExporting cover data from _build/test/cover...~n", []),
    Files = filelib:wildcard("_build/test/cover/*.coverdata"),
    Data = analyze(Files),
    rebar_api:info("exporting ~s~n", [?OUT_FILE]),
    to_json(Data),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Private API
analyze(Files) ->
    try
        cover:start(),
        lists:map(fun(F) ->  cover:import(F),
                             rebar_api:info("importing ~s~n", [F]) end, Files),
        Modules = cover:imported_modules(),
        {result, Result, _} = cover:analyse(Modules, calls, line),
        Result
    catch Error:Reason ->
              rebar_api:abort("~p~n~p~n~p~n",[Error, Reason, erlang:get_stacktrace()])
    end.

to_json(Data) ->
    Mod2Data = lists:foldl(fun add_cover_line_into_array/2, #{}, Data),
    JSON = maps:fold(fun format_array_to_list/3, [], Mod2Data),
    Binary = jiffy:encode(#{<<"coverage">> => {JSON}}),
    file:write_file(?OUT_FILE, Binary).

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
        filename:join("src", Suffix)
    catch Error:Reason ->
              Path = filename:join(["src/", atom_to_list(Module) ++ ".erl"]),
              Issue = io_lib:format("Failed to calculate the source path of module ~p~n
                                     falling back to ~s", [Module, Path]),
              rebar_api:warn("~s~n~p~n~p~n~p~n", [Issue, Error, Reason, erlang:get_stacktrace()]),
              Path
    end.

add_profile_ebin_path(App, State) ->
    ProfileDir = rebar_dir:profile_dir(rebar_state:opts(State), [default, test]),
    Build = rebar_dir:make_relative_path(ProfileDir, rebar_dir:root_dir(State)),
    Beams = filename:join([Build, "lib/", App, "ebin/"]),
    code:add_paths([Beams]).


filter_apps(RawOpts, State) ->
    RawApps = proplists:get_all_values(app, RawOpts),
    Apps = lists:foldl(fun(String, Acc) -> rebar_string:lexemes(String, ",") ++ Acc end, [], RawApps),
    case Apps of
        [] ->
            ProjectApps = rebar_state:project_apps(State),
            lists:map(fun(A) -> erlang:binary_to_atom(rebar_app_info:name(A), utf8) end, ProjectApps);
        _  -> Apps
    end.
