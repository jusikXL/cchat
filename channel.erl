-module(channel).
-behaviour(gen_server).

-export([start/2, stop/1, join/2, leave/2, send_msg/4]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(channel_state, {
    name,
    clients
}).

% interface functions
start(ChannelAtom, Client) ->
    catch (unregister(ChannelAtom)),
    gen_server:start({local, ChannelAtom}, channel, [ChannelAtom, Client], []).

stop(ChannelAtom) ->
    gen_server:cast(ChannelAtom, stop).

join(ChannelAtom, Client) ->
    gen_server:call(ChannelAtom, {join, Client}).

leave(ChannelAtom, Client) ->
    gen_server:call(ChannelAtom, {leave, Client}).

send_msg(ChannelAtom, Client, Nick, Msg) ->
    gen_server:call(ChannelAtom, {send_msg, Client, Nick, Msg}).

% callback functions
init([ChannelAtom, Client]) ->
    InitialState = #channel_state{
        name = atom_to_list(ChannelAtom),
        clients = [Client]
    },
    {ok, InitialState}.

% join
handle_call({join, Client}, _From, State) ->
    case lists:member(Client, State#channel_state.clients) of
        true ->
            % already joined
            {reply, {error, user_already_joined}, State};
        false ->
            % join
            NewState = State#channel_state{
                clients = [Client | State#channel_state.clients]
            },
            {reply, ok, NewState}
    end;
% send message
handle_call({send_msg, Client, Nick, Msg}, _From, State) ->
    case lists:member(Client, State#channel_state.clients) of
        true ->
            [
                genserver:request(
                    C,
                    {message_receive, State#channel_state.name, Nick, Msg}
                )
             || C <- State#channel_state.clients, C =/= Client
            ],

            {reply, ok, State};
        false ->
            {reply, {error, user_not_joined}, State}
    end;
% leave
handle_call({leave, Client}, _From, State) ->
    case lists:member(Client, State#channel_state.clients) of
        true ->
            NewState = State#channel_state{
                clients = lists:delete(Client, State#channel_state.clients)
            },
            {reply, ok, NewState};
        false ->
            {reply, {error, user_not_joined}, State}
    end.

handle_cast(stop, State) ->
    {stop, normal, State}.
