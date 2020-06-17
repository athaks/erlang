-module(foo).
-export([start/0]).


start() ->
    String=lists:flatten(io_lib:format("~9..0B", [1234])),
    lists:flatten(
        io_lib:fwrite("~s-~s-~s~n", [
            string:substr(String,1,3),
            string:substr(String,4,3),
            string:substr(String,7,3)
    ])).
