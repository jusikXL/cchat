-module(server).
-behaviour(gen_server).

-export([start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(server_state, {
    channels,
    users
}).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % + Spawn a new process which waits for a message, handles it, then loops infinitely
    % + Register this process to ServerAtom
    % + Return the process ID
    catch (unregister(ServerAtom)),
    {ok, Pid} = gen_server:start({local, ServerAtom}, server, [], []),
    Pid.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % + Return ok
    gen_server:cast(ServerAtom, stop).

% callback functions
init(_Args) ->
    InitialState = #server_state{
        channels = [],
        users = []
    },
    {ok, InitialState}.

handle_call(_Request, _From, State) ->
    SomeReply = 123,
    NewState = State,
    {reply, SomeReply, NewState}.

handle_cast(stop, State) ->
    {stop, normal, State}.
