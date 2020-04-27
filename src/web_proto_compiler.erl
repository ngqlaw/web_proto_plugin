-module(web_proto_compiler).

-export([compile/2,
         clean/2]).

-define(DEFAULT_PROTO_DIR, "proto").
-define(DEFAULT_OUT_ERL_DIR, "src").
-define(DEFAULT_OUT_HRL_DIR, "include").
-define(DEFAULT_MODULE_PREFIX, "").
-define(DEFAULT_MODULE_SUFFIX, "").

%% ===================================================================
%% Public API
%% ===================================================================

-spec compile(rebar_app_info:t(),
              rebar_state:t()) -> ok.
compile(AppInfo, State) ->
    AppDir = rebar_app_info:dir(AppInfo),
    DepsDir = rebar_dir:deps_dir(State),
    AppOutDir = rebar_app_info:out_dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),
    {ok, GpbOpts0} = dict:find(gpb_opts, Opts),
    {ok, WebProtoOpts0} = dict:find(web_proto_opts, Opts),
    %% check if non-recursive
    Recursive = proplists:get_value(recursive, GpbOpts0, true),
    SourceDirs = proplists:get_all_values(i, GpbOpts0),

    TemplateDirs = proplists:get_all_values(i, WebProtoOpts0),
    TargetErlDir = filename:join([AppOutDir,
                                  proplists:get_value(o, WebProtoOpts0, ?DEFAULT_OUT_ERL_DIR)]),
    rebar_api:debug("making sure that target erl dir ~p exists", [TargetErlDir]),
    ok = ensure_dir(TargetErlDir),
    rebar_api:debug("reading proto files from ~p, generating \".erl\" to ~p "
                    "with \".mustache\" ~p",
                    [SourceDirs, TargetErlDir, TemplateDirs]),
    %% search for .proto files
    FoundProtos = lists:foldl(fun({deps, SourceDir}, Acc) ->
                                      Acc ++ discover(DepsDir, SourceDir, [{recursive, Recursive}]);
                                 (SourceDir, Acc) ->
                                      Acc ++ discover(AppDir, SourceDir, [{recursive, Recursive}])
                              end, [], SourceDirs),
    rebar_api:debug("proto files found~s: ~p",
                    [case Recursive of true -> " recursively"; false -> "" end, FoundProtos]),

    Protos = case proplists:get_value(f, GpbOpts0) of
                 undefined ->
                     FoundProtos;
                 WantedProtos ->
                     rebar_api:debug("Applying filter: ~p", [WantedProtos]),
                     filter_unwanted_protos(WantedProtos, FoundProtos)
             end,
    rebar_api:debug("Filtered protos: ~p", [Protos]),

    %% search for .mustache files
    Templates = lists:foldl(fun({deps, TemplateDir}, Acc) ->
                                    Acc ++ discover(DepsDir, TemplateDir);
                                (TemplateDir, Acc) ->
                                    Acc ++ discover(AppDir, TemplateDir)
                            end, [], TemplateDirs),
    rebar_api:debug("template files found: ~p", [Templates]),

    ModuleNamePrefix = to_binary(proplists:get_value(module_name_prefix, GpbOpts0,
                                           ?DEFAULT_MODULE_PREFIX)),
    ModuleNameSuffix = to_binary(proplists:get_value(module_name_suffix, GpbOpts0,
                                           ?DEFAULT_MODULE_SUFFIX)),
    MainProto = proplists:get_value(main_proto_mark, WebProtoOpts0),
    SubProto = proplists:get_value(sub_proto_mark, WebProtoOpts0),

    %% 遍历所有协议文件，生成模板要素数据
    {MainInfo, SubInfo} = gather(Protos, to_binary(MainProto), to_binary(SubProto), [], #{}),
    rebar_api:debug("gen proto main mark info: ~p", [MainInfo]),
    rebar_api:debug("gen proto sub mark info: ~p", [SubInfo]),
    DataInfo = gen_data_info(MainInfo, SubInfo, ModuleNamePrefix, ModuleNameSuffix, []),
    rebar_api:debug("gen data info: ~p", [DataInfo]),
    %% 使用模板生成文件
    compile(Templates, TargetErlDir, #{"data" => DataInfo}),
    ok.

-spec clean(rebar_app_info:t(),
            rebar_state:t()) -> ok.
clean(AppInfo, State) ->
    AppDir = rebar_app_info:dir(AppInfo),
    DepsDir = rebar_dir:deps_dir(State),
    AppOutDir = rebar_app_info:out_dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),
    {ok, WebProtoOpts0} = dict:find(web_proto_opts, Opts),
    TargetErlDir = filename:join([AppOutDir,
                                  proplists:get_value(o, WebProtoOpts0, ?DEFAULT_OUT_ERL_DIR)]),

    TemplateFiles = find_template_files(AppDir, DepsDir, WebProtoOpts0),
    rebar_api:debug("found template files: ~p", [TemplateFiles]),
    GeneratedRootFiles = [filename:rootname(filename:basename(TemplateFile)) ||
                            TemplateFile <- TemplateFiles],
    GeneratedErlFiles = [filename:join([TargetErlDir, F ++ ".erl"]) ||
                            F <- GeneratedRootFiles],
    rebar_api:debug("deleting [~p]", [GeneratedErlFiles]),
    rebar_file_utils:delete_each(GeneratedErlFiles).

%% ===================================================================
%% Private API
%% ===================================================================
discover(AppDir, SourceDir, Opts) ->
    %% Convert simple extension to proper regex
    SourceExtRe = "^[^._].*\\" ++ ".proto" ++ [$$],

    Recursive = proplists:get_value(recursive, Opts, true),
    %% Find all possible source files
    rebar_utils:find_files(filename:join([AppDir, SourceDir]),
                           SourceExtRe, Recursive).

discover(AppDir, SourceDir) ->
    %% Convert simple extension to proper regex
    SourceExtRe = "^[^._].*\\" ++ ".mustache" ++ [$$],

    %% Find all possible source files
    rebar_utils:find_files(filename:join([AppDir, SourceDir]),
                           SourceExtRe, false).

%% 收集proto文件中的协议号枚举定义
gather([Proto | Rest], Main, Sub, MainInfo, SubInfo) ->
    {ok, FileBin} = file:read_file(Proto),
    {NewMainInfo, NewSubInfo} =
        case do_gather(FileBin, Main, Sub) of
            {main, Content} ->
                L = [{K, binary_to_integer(V)} || {K, V} <- Content],
                {lists:ukeysort(1, L ++ MainInfo), SubInfo};
            {sub, Content} ->
                Name = to_binary(filename:basename(Proto, ".proto")),
                L = [{K, binary_to_integer(V)} || {K, V} <- Content],
                OldL = maps:get(Name, SubInfo, []),
                NewL = lists:ukeysort(1, OldL ++ L),
                {MainInfo, SubInfo#{Name => NewL}};
            not_found ->
                {MainInfo, SubInfo}
        end,
    gather(Rest, Main, Sub, NewMainInfo, NewSubInfo);
gather([], _Main, _Sub, MainInfo, SubInfo) ->
    {MainInfo, SubInfo}.

%% 设置的协议标志在每个proto文件中仅出现一次
do_gather(Bin, MainMark, SubMark) ->
    case web_proto_parse:enum(Bin) of
        {MainMark, Content, _} ->
            {main, Content};
        {SubMark, Content, _} ->
            {sub, Content};
        {_, _, Bin1} ->
            do_gather(Bin1, MainMark, SubMark);
        not_found ->
            not_found
    end.


%% 合并生成数据
gen_data_info([{Name, MainProto} | MainInfo], SubInfo, ModuleNamePrefix, ModuleNameSuffix, DataInfo) ->
    L = maps:get(Name, SubInfo, []),
    NameAtom = binary_to_atom(Name, utf8),
    AddInfo = [#{
        "name" => NameAtom,
        "action" => binary_to_atom(Action, utf8),
        "main_proto_num" => MainProto,
        "sub_proto_num" => SubProto,
        "module" => binary_to_atom(<<ModuleNamePrefix/binary, Name/binary, ModuleNameSuffix/binary>>, utf8)
    } || {Action, SubProto} <- L],
    gen_data_info(MainInfo, SubInfo, ModuleNamePrefix, ModuleNameSuffix, AddInfo ++ DataInfo);
gen_data_info([], _SubInfo, _ModuleNamePrefix, _ModuleNameSuffix, DataInfo) ->
    DataInfo.


compile([TemplatePath | TemplateDir], TargetErlDir, Data) ->
    Template = bbmustache:parse_file(TemplatePath),
    FileBin = bbmustache:compile(Template, Data),
    Filename = target_file(TemplatePath, TargetErlDir),
    file:write_file(Filename, FileBin),
    compile(TemplateDir, TargetErlDir, Data);
compile([], _, _) ->
    ok.


find_first_match(WantedProto, []) ->
    rebar_api:abort("Filtered proto file not found in path: ~p", [WantedProto]);

find_first_match(WantedProto, [Head | RemainingProtos]) ->
    case is_wanted_proto(WantedProto, Head)of
        true ->
            Head;
        false ->
            find_first_match(WantedProto, RemainingProtos)
    end.


is_wanted_proto(WantedProto, ProtoPath) ->
    case string:find(ProtoPath, WantedProto, trailing) of
        nomatch -> false;
        _Match -> true
    end.

filter_unwanted_protos(WantedProtos, AllProtos) ->
    [find_first_match(WantedProto, AllProtos) || WantedProto <- WantedProtos].


target_file(Template, TargetErlDir) ->
    Module = filename:basename(Template, ".mustache"),
    filename:join([TargetErlDir, Module ++ ".erl"]).


-spec ensure_dir(filelib:dirname()) -> 'ok' | {error, Reason::file:posix()}.
ensure_dir(OutDir) ->
  %% Make sure that ebin/ exists and is on the path
  case filelib:ensure_dir(filename:join(OutDir, "dummy.beam")) of
    ok -> ok;
    {error, eexist} ->
      rebar_utils:abort("unable to ensure dir ~p, is it maybe a broken symlink?",
        [OutDir]);
    {error, Reason} -> {error, Reason}
  end.


find_template_files(AppDir, DepsDir, Opts) ->
    lists:foldl(fun({deps, SourceDir}, Acc) ->
                    Acc ++ rebar_utils:find_files(
                             filename:join(DepsDir, SourceDir),
                                           ".*\.mustache\$");
                   (SourceDir, Acc) ->
                    Acc ++ rebar_utils:find_files(
                             filename:join(AppDir, SourceDir),
                                           ".*\.mustache\$")
                end,
                [], proplists:get_all_values(i, Opts)).


to_binary(S) when is_list(S) -> list_to_binary(S);
to_binary(B) when is_binary(B) -> B.
