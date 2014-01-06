%% @author Evgeny Pashkin
%% @doc Proxy for module file for test mocking

-module(servant_file_proxy).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

list_dir_all(Dir) ->
    file:list_dir_all(Dir).

rename(Source, Destination) ->
    file:rename(Source, Destination).

del_dir(Dir) ->
    file:del_dir(Dir).
