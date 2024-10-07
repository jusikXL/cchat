% Reuse error handling logic.
-module(utils).

-export([gen_server_call/2, gen_server_call/3]).

% Uses a shorter timeout so that tests don't timeout
% Both gen_server and tests default to 5000
gen_server_call(ServerAtom, Msg) ->
    gen_server_call(ServerAtom, Msg, 3000).

% Responds with `server_not_reached` when server is unavailable
gen_server_call(ServerAtom, Msg, Timeout) ->
    try gen_server:call(ServerAtom, Msg, Timeout) of
        Result -> Result
    catch
        exit:_ -> {error, server_not_reached};
        error:noproc -> {error, server_not_reached}
    end.
