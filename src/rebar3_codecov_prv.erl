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
                      rebar_api:info("importing ~s~n", [F])
                  end,
                  Files),
        Modules = cover:imported_modules(),
        {result, Result, _} = cover:analyse(Modules, calls, line),
        Result
    catch
        Error:Reason:Stacktrace ->
            rebar_api:abort("~p~n~p~n~p~n",[Error, Reason, Stacktrace])
    end.

export(Data, State) ->
    Mod2Data = lists:foldl(fun add_cover_line_into_array/2, #{}, Data),
    Formats = export_formats(State),
    to_json(Mod2Data, Formats),
    to_lcov(Mod2Data, Formats).

export_formats(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    LCov = proplists:get_value(lcov, Args, false),
    JSON = proplists:get_value(json, Args, true),
    #{lcov => LCov, json => JSON}.

to_json(Mod2Data, #{json := true}) ->
    rebar_api:info("exporting ~s~n", [?JSON_OUT_FILE]),
    JSON = maps:fold(fun format_array_to_list/3, [], Mod2Data),
    Binary = jiffy:encode(#{<<"coverage">> => {JSON}}),
    file:write_file(?JSON_OUT_FILE, Binary);
to_json(_, _) -> ok.

to_lcov(Mod2Data, #{lcov := true}) ->
    rebar_api:info("exporting ~s~n", [?LCOV_OUT_FILE]),
    {ok, LCovFile} = file:open(?LCOV_OUT_FILE, [write]),
    maps:fold(fun module_to_lcov/3, LCovFile, Mod2Data),
    file:close(LCovFile);
to_lcov(_, _) -> ok.

add_cover_line_into_array({{Module, Line}, CallTimes}, Acc) ->
    CallsPerLineArray = maps:get(Module, Acc, array:new({default, null})),
    Acc#{Module => array:set(Line, CallTimes, CallsPerLineArray)}.

format_array_to_list(Module, CallsPerLineArray, Acc) ->
    ListOfCallTimes = array:to_list(CallsPerLineArray),
    BinPath = list_to_binary(get_source_path(Module)),
    [{BinPath, ListOfCallTimes}|Acc].

module_to_lcov(Module, CallsPerLineArray, LCovFile) ->
    %% lcov file format description can be found here:
    %%    https://manpages.debian.org/stretch/lcov/geninfo.1.en.html#FILES
    %% currently this generator creates only the list of execution counts
    %% for each instrumented line, which is enough for coveralls service.
    io:format(LCovFile, "SF:~s~n", [get_source_path(Module)]),
    CallTimes = array:to_orddict(CallsPerLineArray),
    InstrumentedLines = [{N, C} || {N, C} <- CallTimes, C =/= null],
    ExecutedLines = [{N, C} || {N, C} <- InstrumentedLines, C > 0],
    [io:format(LCovFile, "DA:~p,~p~n", [N, C]) || {N, C} <- InstrumentedLines],
    io:format(LCovFile, "LH:~p~n", [length(ExecutedLines)]),
    io:format(LCovFile, "LF:~p~n", [length(InstrumentedLines)]),
    io:format(LCovFile, "end_of_record~n", []),
    LCovFile.

get_source_path(Module) when is_atom(Module) ->
    Name = atom_to_list(Module)++".erl",
    try filelib:wildcard([filename:join(["src/**/", Name])]) of
        [P] -> P;
        _ ->
            Issue = io_lib:format("Failed to calculate the source path of module ~p~n", [Module]),
            rebar_api:warn("~s~n", [Issue]),
            []
    catch
        Error:Reason:Stacktrace ->
            Issue = io_lib:format("Failed to calculate the source path of module ~p~n
                                     falling back to ~s", [Module, Name]),
            rebar_api:warn("~s~n~p~n~p~n~p~n", [Issue, Error, Reason, Stacktrace]),
            Name
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
    Opts = rebar_app_info:opts(AppInfo),
    CoverDataPath = case dict:find(codecov_opts, Opts) of
                        {ok, CodecovOpts} ->
                            Paths = proplists:get_value(path, CodecovOpts, ["_build/test/cover"]),
                            lists:map(fun(P) -> filename:join([P, "*.coverdata"]) end, Paths);
                        _ ->
                            ["_build/test/cover/*.coverdata"]
                    end,
    rebar_api:info("Exporting cover data from ~p~n", [CoverDataPath]),
    lists:flatmap(fun filelib:wildcard/1, CoverDataPath).
