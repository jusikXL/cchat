-module(ch3).
-behaviour(gen_server).

-export([start/0, stop/0]).
-export([alloc/0, free/1]).
-export([init/1, handle_call/3, handle_cast/2]).

% interface functions

start() ->
    gen_server:start({local, ch3}, ch3, [], []).

stop() ->
    gen_server:cast(ch3, stop).

alloc() ->
    gen_server:call(ch3, alloc).

free(Ch) ->
    gen_server:cast(ch3, {free, Ch}).

% callback functions

init(_Args) ->
    {ok, channels()}.

handle_call(alloc, _From, Chs) ->
    {Ch, Chs2} = alloc(Chs),
    {reply, Ch, Chs2}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({free, Ch}, Chs) ->
    Chs2 = free(Ch, Chs),
    {noreply, Chs2}.

% terminate(normal, State) ->
%     ok.

% implementation

channels() ->
    {_Allocated = [], _Free = lists:seq(1, 100)}.

alloc({Allocated, [H | T] = _Free}) ->
    {H, {[H | Allocated], T}}.

free(Ch, {Alloc, Free} = Channels) ->
    case lists:member(Ch, Alloc) of
        true ->
            {lists:delete(Ch, Alloc), [Ch | Free]};
        false ->
            Channels
    end.
