% An implementation of `gen_server` that represents a cchat server.
% Responsible for keeping track of channels and forwarding join requests to the correct channel.
% Delegates rest of the logic to the channel server.
-module(server).
-behaviour(gen_server).

-export([start/1, stop/1, join/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(server_state, {
    channels
}).

%%% INTERFACE

% Starts the server
start(ServerAtom) ->
    catch (unregister(ServerAtom)),
    {ok, Pid} = gen_server:start({local, ServerAtom}, server, [], []),
    Pid.

% Stops the server
stop(ServerAtom) ->
    utils:gen_server_call(ServerAtom, {stop}).

% Joins a channel, is called from the client 
join(ServerAtom, ChannelAtom) ->
    utils:gen_server_call(ServerAtom, {join, ChannelAtom}).

%%% GEN_SERVER CALLBACKS

% Initializes the server with no active channels
init(_Args) ->
    InitialState = #server_state{
        channels = []
    },
    {ok, InitialState}.

% Handle join
handle_call({join, ChannelAtom}, {Client, _Reply}, State) ->
    case lists:member(ChannelAtom, State#server_state.channels) of
        % Channel has already been created
        true ->
            case channel:join(ChannelAtom, Client) of
                {error, user_already_joined} ->
                    {reply, {error, user_already_joined}, State};
                ok ->
                    {reply, ok, State}
            end;
        % Channel does not exist
        false ->
            % Create channel, the client is automatically added to the channel on creation
            channel:start(ChannelAtom, Client),
            % Keep track of active channels in the server state
            NewState = State#server_state{channels = [ChannelAtom | State#server_state.channels]},
            {reply, ok, NewState}
    end;
% Handle stop
handle_call({stop}, _From, State) ->
    [
        channel:stop(C)
     || C <- State#server_state.channels
    ],
    {stop, normal, State}.

% We do not have any casts to handle
handle_cast(_Request, State) ->
    {noreply, State}.
