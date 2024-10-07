% An implementation of `gen_server` that represents a channel.
-module(channel).
-behaviour(gen_server).

-export([start/2, stop/1, join/2, leave/1, send_msg/3]).
-export([init/1, handle_call/3, handle_cast/2]).

% Record that defines the state of a channel 
-record(channel_state, {
    % Name of the channel, derived from the registered atom
    name,
    % A list of user clients that are listening to the channel
    clients
}).

%%% INTERFACE

% Starts the server
start(ChannelAtom, Client) ->
    catch (unregister(ChannelAtom)),
    gen_server:start({local, ChannelAtom}, channel, [ChannelAtom, Client], []).

% Stops the server
stop(ChannelAtom) ->
    utils:gen_server_call(ChannelAtom, {stop}).

% Adds a user to the channels list of clients
join(ChannelAtom, Client) ->
    utils:gen_server_call(ChannelAtom, {join, Client}).

% Removes a user from the channels list of clients
leave(ChannelAtom) ->
    utils:gen_server_call(ChannelAtom, {leave}).

% Sends a message to all clients in the channel
send_msg(ChannelAtom, Nick, Msg) ->
    utils:gen_server_call(ChannelAtom, {send_msg, Nick, Msg}).

%%% GEN_SERVER CALLBACKS 

% Initializes the channel server, adds the client to creates the channel immediately
init([ChannelAtom, Client]) ->
    InitialState = #channel_state{
        name = atom_to_list(ChannelAtom),
        clients = [Client]
    },
    {ok, InitialState}.

% Handle join
handle_call({join, Client}, _From, State) ->
    case lists:member(Client, State#channel_state.clients) of
        true ->
            {reply, {error, user_already_joined}, State};
        false ->
            % Add client to channel
            NewState = State#channel_state{
                clients = [Client | State#channel_state.clients]
            },
            {reply, ok, NewState}
    end;
% Handle send message
handle_call({send_msg, Nick, Msg}, {Client, _Reply}, State) ->
    case lists:member(Client, State#channel_state.clients) of
        true ->
            % Spawn another process to create the request
            % Increases message throughput, since `genserver:request/2` waits for response from client
            spawn(fun() ->
                % Iterates over each client and sends a message receive request to them
                [
                    genserver:request(
                        C,
                        {message_receive, State#channel_state.name, Nick, Msg}
                    )
                 || C <- State#channel_state.clients, C =/= Client % Omit the client who sent the message themselves
                ]
            end),

            {reply, ok, State};
        false ->
            {reply, {error, user_not_joined}, State}
    end;
% Handle leave
handle_call({leave}, {Client, _Reply}, State) ->
    case lists:member(Client, State#channel_state.clients) of
        true ->
            % Remove client from channel
            NewState = State#channel_state{
                clients = lists:delete(Client, State#channel_state.clients)
            },
            {reply, ok, NewState};
        false ->
            {reply, {error, user_not_joined}, State}
    end;
% Handle stop
handle_call({stop}, _From, State) ->
    {stop, normal, State}.

% We do not have any casts to handle
handle_cast(_Request, State) ->
    {noreply, State}.
