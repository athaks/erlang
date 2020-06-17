-module(rhCerts).
-export( [start/0, start/1, verifyServer/1] ).

% erl -noshell -s rhCerts start -s init stop
start() -> 
    io:format("2 arguments are required:~n"++
              "first last~n"++
              " e.g. 110320421 110320447~n"),
    exit(normal).

start( Arguments ) ->
    io:format("~p~n", [Arguments(0)]),
    First = 1,
    Last = 1,
    Pool = [
        spawn(rhCerts, verifyServer, [self()]),
        spawn(rhCerts, verifyServer, [self()]),
        spawn(rhCerts, verifyServer, [self()])
    ],
    io:format("~p Pool ready: ~p~n", [self(), Pool]),
    serveNext(First, Last).

serveNext(First, Last) ->
    receive
        { done, Who } ->
            io:format("~p ~s finished~n", [self(), Who]);
        { next, Who } ->
            io:format("~p ~p requested work~n", [self(), Who]),
            Who ! { next, num2cert(First) },
            if (First>Last) ->
                serveNext(First+1, Last)
            end
    end,
    io:format("~p Server terminating.~n", [self()] ).

num2cert(Num) ->
    String=lists:flatten(io_lib:format("~9..0B", [Num])),
    lists:flatten(
        io_lib:fwrite("~s-~s-~s~n", [
            string:substr(String,1,3),
            string:substr(String,4,3),
            string:substr(String,7,3)
    ])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verifyServer(Master) ->
    verifyNext(Master, prepare()).

verifyNext(Master, RegExp) ->
    io:format("~p Requesting from server ~p~n", [self(), Master]),
    Master ! { next, self() },
    receive
        { next, Certificate } ->
            io:format("~p ~s received.~n", [self(), Certificate]),
            check(Certificate, RegExp);
            %TODO verifyNext(Master, RegExp);
        { done } ->
            Master ! { done, self() },
            io:format("~p Client terminated~n", [self()])
    end,
    io:format("~p Client terminating~n", [self()]).
    
test() -> 
    check("130-072-275", prepare()).

prepare() ->
    io:format("~p Prepaging~n", [self()] ),
    RegExp=regexp(),
    case inets:start(transient) of
        ok ->
            io:fwrite("~p inets::ok ~n", [self()]);
        {error, inetsReason} ->
            io:fwrite("~p INETS:ERROR ~p~n", [self(), inetsReason])
    end,
    case ssl:start(transient) of
        ok ->
            io:fwrite("~p ssl::ok ~n", [self()]);
        {error, sslReason} ->
            io:fwrite("~p SSL:ERROR ~p~n", [self(), sslReason])
    end,
    RegExp.

check(Certificate, RegExp) ->
    case verify(Certificate, RegExp) of
        invalid ->
            %io:format("~s Invalid\n", [Certificate]);
            invalid;
        {ok, Owner} ->
            io:format("~p ~s ~s\n", [self(), Certificate, Owner])
    end.

regexp() ->
    %io:fwrite("RE Compiling... "),
    case re:compile(
        ".*<td>\\s+Owner:\\s+</td>\\s+<td>"++
        "\\s+(.+?)\\s+</td>.*",
        [multiline]
    ) of
        {ok, RegExp} ->
            %io:fwrite("RE valid\n"),
            RegExp
    end.

verify(Certificate, RegExp) ->
    URL="https://www.redhat.com/wapps/training/"++
        "certification/verify.html?certNumber="++
        Certificate,
    io:fwrite("~p Requesting ...~n", [self()]),
    case httpc:request(URL) of
        {ok, { _, _, Body}} ->
            io:fwrite("~p Validating ...~n", [self()] ),
            case re:run(Body,
                        RegExp,
                        [ {capture, all_but_first, list} ]
            ) of
                { match, Found } ->
                    %io:fwrite(Found++"\n"),
                    {ok, Found};
                nomatch ->
                    %io:fwrite("Invalid.\n"),
                    invalid;
                true ->
                    io:fwrite("~p .....................~n", [self()])
            end;
        {error, Reason} ->
            io:fwrite("~p ERROR ~p~n", [self(), Reason]);
        true ->
            io:fwrite("~p ?????????????????????~n", [self()])
    end.
