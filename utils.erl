-module(utils).

-export([gen_server_call/2, gen_server_call/3]).

gen_server_call(ServerAtom, Msg) ->
    gen_server_call(ServerAtom, Msg, 3000).

gen_server_call(ServerAtom, Msg, Timeout) ->
    try gen_server:call(ServerAtom, Msg, Timeout) of
        Result -> Result
    catch
        exit:_ -> {error, server_not_reached};
        error:noproc -> {error, server_not_reached}
    end.
