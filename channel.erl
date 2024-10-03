-module(channel).
-behaviour(gen_server).

-export([start/2, stop/1, join/2, send_msg/4]).
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

% leave(ChannelAtom, Client) ->
%     gen_server:call(ChannelAtom, {leave, Client}).

send_msg(ChannelAtom, Client, Nick, Msg) ->
    io:fwrite("~p~n", ["channel interface"]),
    gen_server:call(ChannelAtom, {send_msg, Client, Nick, Msg}).

% callback functions
init([ChannelAtom, Client]) ->
    InitialState = #channel_state{
        name = atom_to_list(ChannelAtom),
        clients = [Client]
    },
    {ok, InitialState}.

handle_call({join, Client}, _Server, State) ->
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
handle_call({send_msg, Client, Nick, Msg}, _Server, State) ->
    io:fwrite("~p~n", ["channel callback"]),
    case lists:member(Client, State#channel_state.clients) of
        true ->
            io:fwrite("~p~n", [{message_receive, State#channel_state.name, Nick, Msg}]),
            lists:foreach(
                fun(C) ->
                    genserver:request(
                        C,
                        {message_receive, State#channel_state.name, Nick, Msg}
                    )
                end,
                State#channel_state.clients
            ),

            % [
            %     genserver:request(
            %         C,
            %         {message_receive, "#test", Nick, Msg}
            %     )
            %  || C <- State#channel_state.clients
            % ],

            {reply, ok, State};
        false ->
            {reply, {error, user_not_joined}, State}
    end.

handle_cast(stop, State) ->
    {stop, normal, State}.
