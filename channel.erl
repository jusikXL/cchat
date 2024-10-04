-module(channel).
-behaviour(gen_server).

-export([start/2, stop/1, join/2, leave/1, send_msg/3]).
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
    utils:gen_server_call(ChannelAtom, {stop}).

join(ChannelAtom, Client) ->
    utils:gen_server_call(ChannelAtom, {join, Client}).

leave(ChannelAtom) ->
    utils:gen_server_call(ChannelAtom, {leave}).

send_msg(ChannelAtom, Nick, Msg) ->
    utils:gen_server_call(ChannelAtom, {send_msg, Nick, Msg}).

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
handle_call({send_msg, Nick, Msg}, {Client, _Reply}, State) ->
    case lists:member(Client, State#channel_state.clients) of
        true ->
            spawn(fun() ->
                [
                    genserver:request(
                        C,
                        {message_receive, State#channel_state.name, Nick, Msg}
                    )
                 || C <- State#channel_state.clients, C =/= Client
                ]
            end),

            {reply, ok, State};
        false ->
            {reply, {error, user_not_joined}, State}
    end;
% leave
handle_call({leave}, {Client, _Reply}, State) ->
    case lists:member(Client, State#channel_state.clients) of
        true ->
            NewState = State#channel_state{
                clients = lists:delete(Client, State#channel_state.clients)
            },
            {reply, ok, NewState};
        false ->
            {reply, {error, user_not_joined}, State}
    end;
handle_call({stop}, _From, State) ->
    {stop, normal, State}.

% unused
handle_cast(_Request, State) ->
    {noreply, State}.
