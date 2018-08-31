-module(rebar3_codecov_helper).

-export([analyze/1]).

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
        string_prefix(AbsPath, get_repo_dir(Module))
    catch Error:Reason ->
        Stacktrace = erlang:get_stacktrace(),
        error_logger:warning_msg("issue=get_source_path_failed module=~p "
                                  "reason=~p:~p stacktrace=~1000p",
                                 [Module, Error, Reason, Stacktrace]),
        atom_to_list(Module) ++ ".erl"
    end.

get_repo_dir(Module) ->
    RepoPath = proplists:get_value(source, Module:module_info(compile)),
    Suffix = "src/" ++ atom_to_list(Module) ++ ".erl",
    string_suffix(RepoPath, Suffix).

string_prefix([H|String], [H|Prefix]) ->
    string_prefix(String, Prefix);
string_prefix(String, []) ->
    String.

string_suffix(String, Suffix) ->
    StringR = lists:reverse(String),
    SuffixR = lists:reverse(Suffix),
    lists:reverse(string_prefix(StringR, SuffixR)).


analyze(Args) ->
    try
        [InFile, OutFile] = Args,
        cover:start(),
        io:format("importing ~s~n", [InFile]),
        ok = cover:import(InFile),
        io:format("exporting ~s~n", [OutFile]),
        to_json(OutFile),
        init:stop(0)
    catch E:R ->
              io:format("~p:~p~n", [E, R]),
              io:format("~p", [erlang:get_stacktrace()]),
              init:stop(1)
    end.
