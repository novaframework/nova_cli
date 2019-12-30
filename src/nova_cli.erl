-module(nova_cli).
-export([
         create_page/4,
         create_page/5,
         create_rest_controller/4,
         create_rest_controller/5
        ]).

-include_lib("nova/include/nova.hrl").


create_page(MainApp, Route, Module, Function) ->
    create_page(MainApp, Route, Module, Function, #{}).

create_page(MainApp, Route, Module, Function, Options) ->
    RouteFile = filename:join([code:lib_dir(MainApp, priv), atom_to_list(MainApp) ++ ".routes.erl"]),

    Prefix = atom_to_list(MainApp) ++ "_" ++ atom_to_list(Module),

    ControllerFile = filename:join([code:lib_dir(MainApp, src), "controllers/", Prefix ++ "_controller.erl"]),
    {ok, FP} = file:open(ControllerFile, [write]),
    ModBin = atom_to_binary(Module, utf8),
    FuncBin = atom_to_binary(Function, utf8),
    ok = file:write(FP, [<<"-module(">>, list_to_binary(Prefix), <<"_controller).\n-export([">>, FuncBin, <<"/1]).\n\n">>, FuncBin, <<"(Req) ->\n    {ok, []}.">>]),
    file:close(FP),

    ViewFile = filename:join([code:lib_dir(MainApp, src), "views/", Prefix ++ ".dtl"]),
    {ok, FP2} = file:open(ViewFile, [write]),
    ok = file:write(FP2, [<<"<html>\n  <body>\n    <h1>Bonjour mademoiselle</h1>\n  </body>\n</html>">>]),
    file:close(FP2),

    %% Build the route info
    RouteInfo = #{application => MainApp,
                  prefix => maps:get(prefix, Options, ""),
                  host => maps:get(host, Options, '_'),
                  security => maps:get(security, Options, false)},
    nova_router:add_route(RouteInfo, {Route, {Module, Function}, Options}),

    RouteTerm = io_lib:format("~tp.~n", [RouteInfo#{routes => [{Route, {Module, Function}, Options}]}]),
    {ok, FP3} = file:open(RouteFile, [append]),
    ok = file:write(FP3, list_to_binary(RouteTerm)),
    file:close(FP3),
    ?INFO("Added new page for route ~p.~nController: ~p~nView: ~p", [Route, ControllerFile, ViewFile]).


create_rest_controller(MainApp, Route, Module, Function) ->
    create_rest_controller(MainApp, Route, Module, Function, #{}).

create_rest_controller(MainApp, Route, Module, Function, Options) ->
    RouteFile = filename:join([code:lib_dir(MainApp, priv), MainApp, ".routes.erl"]),
    Prefix = atom_to_list(MainApp) ++ "_" ++ atom_to_list(Module),

    ControllerFile = filename:join([code:lib_dir(MainApp, src), "controllers/", Prefix ++ "_controller.erl"]),
    {ok, FP} = file:open(ControllerFile, [write]),
    ModBin = atom_to_binary(Module, utf8),
    FuncBin = atom_to_binary(Function, utf8),
    ok = file:write(FP, [<<"-module(">>, list_to_binary(Prefix), <<"_controller).\n-export([">>, FuncBin, <<"/2]).\n\n">>, FuncBin, <<"(Req) ->\n    {json, [#{success => true}]}.">>]),
    file:close(FP),

    %% Build the route info
    RouteInfo = #{application => MainApp,
                  prefix => maps:get(prefix, Options, ""),
                  host => maps:get(host, Options, '_'),
                  security => maps:get(security, Options, false)},
    nova_router:add_route(RouteInfo, {Route, {Module, Function}, Options}),

    RouteTerm = io_lib:format("~tp.~n", [RouteInfo#{routes => [{Route, {Module, Function}, Options}]}]),
    {ok, FP3} = file:open(RouteFile, [append]),
    ok = file:write(FP3, list_to_binary(RouteTerm)),
    file:close(FP3),

    ?INFO("Added new rest controller for route ~p.~nController: ~p", [Route, ControllerFile]).
