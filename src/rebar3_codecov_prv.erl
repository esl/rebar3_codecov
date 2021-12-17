-module(rebar3_codecov_prv).
-behaviour(provider).

%% API
-export([init/1, do/1, format_error/1]).

-define(NAMESPACE, codecov).
-define(PROVIDER, analyze).
-define(DEPS, [{default, app_discovery}]).
-define(DESC, "Converts .coverdata files to codecov compatible JSON").
-define(JSON_OUT_FILE, "codecov.json").
-define(LCOV_OUT_FILE, "lcov.info").

%% Public API
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                    {name, ?PROVIDER},
                                    {namespace, ?NAMESPACE},
                                    {module, ?MODULE},
                                    {bare, true},
                                    {deps, ?DEPS},
                                    {example, "rebar3 codecov analyze --lcov --json false"},
                                    {opts, [
                                        {lcov, undefined, "lcov", boolean,
                                         "export data in lcov format (false by default)"},
                                        {json, undefined, "json", boolean,
                                         "export data in json format (true by default)"}
                                    ]},
                                    {short_desc, ?DESC},
                                    {desc, ?DESC}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    Apps = filter_apps(RawOpts, State),
    lists:map(fun(App) ->
                  add_profile_ebin_path(App, State),
                  add_app_ebin_path(App)
              end, Apps),
    AppsInfo = rebar_state:project_apps(State),
    Defaults = lists:map(fun(App) ->
                             EBin = rebar_app_info:ebin_dir(App),
                             rebar_dir:make_relative_path(EBin, rebar_dir:root_dir(State))
                         end, AppsInfo),
    code:add_paths(Defaults),
    Files = lists:flatmap(fun get_coverdata_files/1, AppsInfo),
    Data = analyze(Files),
    export(Data, State),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Private API
analyze(Files) ->
    try
        cover:start(),
        lists:map(fun(F) ->
                      cover:import(F),
                      rebar_api:info("importing ~s", [F])
                  end,
                  Files),
        Modules = cover:imported_modules(),
        {result, Result, _} = cover:analyse(Modules, calls, line),
        Result
    catch
        Error:Reason:Stacktrace ->
            rebar_api:abort("~p~n~p~n~p",[Error, Reason, Stacktrace])
    end.

export(Data, State) ->
    Mod2Data = lists:foldl(fun add_cover_line_into_array/2, #{}, Data),
    Formats = export_formats(State),
    SrcPaths = source_paths(State),
    to_json(Mod2Data, Formats, SrcPaths),
    to_lcov(Mod2Data, Formats, SrcPaths).

export_formats(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    LCov = proplists:get_value(lcov, Args, false),
    JSON = proplists:get_value(json, Args, true),
    #{lcov => LCov, json => JSON}.

to_json(Mod2Data, #{json := true}, SrcPaths) ->
    rebar_api:info("exporting ~s", [?JSON_OUT_FILE]),
    JSON = maps:fold(fun(Module, Calls, Acc) ->
                             format_array_to_list(Module, Calls, Acc, SrcPaths)
                     end, [], Mod2Data),
    Binary = jiffy:encode(#{<<"coverage">> => {JSON}}),
    file:write_file(?JSON_OUT_FILE, Binary);
to_json(_, _, _) -> ok.

to_lcov(Mod2Data, #{lcov := true}, SrcPaths) ->
    rebar_api:info("exporting ~s", [?LCOV_OUT_FILE]),
    {ok, LCovFile} = file:open(?LCOV_OUT_FILE, [write]),
    maps:fold(fun(Module, Calls, Acc) ->
                      module_to_lcov(Module, Calls, Acc, SrcPaths)
              end, LCovFile, Mod2Data),
    file:close(LCovFile);
to_lcov(_, _, _) -> ok.

add_cover_line_into_array({{Module, Line}, CallTimes}, Acc) ->
    CallsPerLineArray = maps:get(Module, Acc, array:new({default, null})),
    Acc#{Module => array:set(Line, CallTimes, CallsPerLineArray)}.

format_array_to_list(Module, CallsPerLineArray, Acc, SrcPaths) ->
    ListOfCallTimes = array:to_list(CallsPerLineArray),
    BinPath = list_to_binary(get_source_path(Module, SrcPaths)),
    [{BinPath, ListOfCallTimes}|Acc].

module_to_lcov(Module, CallsPerLineArray, LCovFile, SrcPaths) ->
    %% lcov file format description can be found here:
    %%    https://manpages.debian.org/stretch/lcov/geninfo.1.en.html#FILES
    %% currently this generator creates only the list of execution counts
    %% for each instrumented line, which is enough for coveralls service.
    io:format(LCovFile, "SF:~s~n", [get_source_path(Module, SrcPaths)]),
    CallTimes = array:to_orddict(CallsPerLineArray),
    InstrumentedLines = [{N, C} || {N, C} <- CallTimes, C =/= null],
    ExecutedLines = [{N, C} || {N, C} <- InstrumentedLines, C > 0],
    [io:format(LCovFile, "DA:~p,~p~n", [N, C]) || {N, C} <- InstrumentedLines],
    io:format(LCovFile, "LH:~p~n", [length(ExecutedLines)]),
    io:format(LCovFile, "LF:~p~n", [length(InstrumentedLines)]),
    io:format(LCovFile, "end_of_record~n", []),
    LCovFile.


source_paths(State) ->
    AppsInfo = rebar_state:project_apps(State),
    maps:from_list(lists:flatmap(fun source_paths_for_app/1, AppsInfo)).

source_paths_for_app(AppInfo) ->
    CodecovOpts = get_codecov_opts(AppInfo),
    AppDir = rebar_app_info:dir(AppInfo),
    SrcPaths = proplists:get_value(source_paths, CodecovOpts, ["src/**"]),
    Paths = lists:append([filelib:wildcard(filename:join(AppDir, SrcPath)) || SrcPath <- SrcPaths]),
    rebar_api:info("Found ~p paths in ~p", [length(Paths), AppDir]),
    lists:flatmap(fun prepare_path/1, Paths).

prepare_path(Path) ->
    case lists:suffix(".erl", Path) of
        true ->
            Module = list_to_atom(filename:basename(Path, ".erl")),
            [{Module, Path}];
        false ->
            []
    end.

get_source_path(Module, SrcPaths) when is_atom(Module) ->
    case maps:get(Module, SrcPaths, undefined) of
        undefined ->
            Name = atom_to_list(Module) ++ ".erl",
            rebar_api:warn("Failed to calculate the source path of module ~p~n"
                           "falling back to ~s", [Module, Name]),
            Name;
        P ->
            P
    end.

add_profile_ebin_path(App, State) ->
    ProfileDir = rebar_dir:profile_dir(rebar_state:opts(State), [default, test]),
    Build = rebar_dir:make_relative_path(ProfileDir, rebar_dir:root_dir(State)),
    Beams = filename:join([Build, "lib/", App, "ebin/"]),
    code:add_paths([Beams]).

add_app_ebin_path(App) ->
    Wildcard = filename:join(["_build/**/lib", App, "ebin"]),
    Paths = filelib:wildcard(Wildcard),
    code:add_paths(Paths).

filter_apps(RawOpts, State) ->
    RawApps = proplists:get_all_values(app, RawOpts),
    Apps = lists:foldl(fun(String, Acc) -> rebar_string:lexemes(String, ",") ++ Acc end, [], RawApps),
    case Apps of
        [] ->
            ProjectApps = rebar_state:project_apps(State),
            lists:map(fun(A) -> erlang:binary_to_atom(rebar_app_info:name(A), utf8) end, ProjectApps);
        _  -> Apps
    end.

get_coverdata_files(AppInfo) ->
    CodecovOpts = get_codecov_opts(AppInfo),
    AppDir = rebar_app_info:dir(AppInfo),
    Paths = proplists:get_value(path, CodecovOpts, ["_build/test/cover"]),
    CoverDataPath = lists:map(fun(P) -> filename:join([AppDir, P, "*.coverdata"]) end, Paths),
    rebar_api:info("Exporting cover data from ~p", [CoverDataPath]),
    lists:flatmap(fun filelib:wildcard/1, CoverDataPath).

get_codecov_opts(AppInfo) ->
    Opts = rebar_app_info:opts(AppInfo),
    case dict:find(codecov_opts, Opts) of
        {ok, CodecovOpts} ->
            CodecovOpts;
        _ ->
            []
    end.
