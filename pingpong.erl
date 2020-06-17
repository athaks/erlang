-module(pingpong).
-export([start/0, ping/2, pong/2]).

ping(0, Pong_PID) ->
    Pong_PID ! finished,
    io:format("... ping ~p finished~n", [self()]);
ping(N, Pong_PID) ->
    Pong_PID ! {ping, self()},
    receive
        pong ->
            ok
            %io:format(">>> Ping ~p received pong from ~p [~p]~n", [self(), Pong_PID, N])
    end,
    ping(N - 1, Pong_PID).

pong(N, StartTime) ->
    receive
        finished ->
            io:format("!!! Pong ~p finished [~p] [~p]~n", [self(), N, timer:now_diff(StartTime, now())]),
            pong(N+1, StartTime);
        {ping, Ping_PID} ->
            %io:format("    Pong ~p received ping from ~p [~p]~n", [self(), Ping_PID, N]),
            Ping_PID ! pong,
            pong(N+1, StartTime)
    end.

start() ->
    Pong_PID = spawn(pingpong, pong, [1, now()]),
    spawn(pingpong, ping, [999999, Pong_PID]),
    spawn(pingpong, ping, [999999, Pong_PID]),
    spawn(pingpong, ping, [999999, Pong_PID]),
    spawn(pingpong, ping, [999999, Pong_PID]),
    spawn(pingpong, ping, [999999, Pong_PID]),
    spawn(pingpong, ping, [999999, Pong_PID]),
    spawn(pingpong, ping, [999999, Pong_PID]),
    spawn(pingpong, ping, [999999, Pong_PID]),
    spawn(pingpong, ping, [999999, Pong_PID]),
    spawn(pingpong, ping, [999999, Pong_PID]).
