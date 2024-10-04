-module(server).
-behaviour(gen_server).

-export([start/1, stop/1, join/2, leave/2, send_msg/4]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(server_state, {
    channels
}).

% interface functions
start(ServerAtom) ->
    catch (unregister(ServerAtom)),
    {ok, Pid} = gen_server:start({local, ServerAtom}, server, [], []),
    Pid.

stop(ServerAtom) ->
    gen_server:cast(ServerAtom, stop).

join(ServerAtom, ChannelAtom) ->
    try gen_server:call(ServerAtom, {join, ChannelAtom}, 1000) of
        Result -> Result
    catch 
        exit:_ -> {error, server_not_reached}
    end.

leave(ServerAtom, ChannelAtom) ->
    gen_server:call(ServerAtom, {leave, ChannelAtom}).

send_msg(ServerAtom, ChannelAtom, Nick, Msg) ->
    try gen_server:call(ServerAtom, {send_msg, ChannelAtom, Nick, Msg}) of
        Result -> Result
    catch 
        exit:_ -> {error, server_not_reached};
        noproc -> {error, server_not_reached}
    end.

% callback functions
init(_Args) ->
    InitialState = #server_state{
        channels = []
    },
    {ok, InitialState}.

handle_call({send_msg, ChannelAtom, Nick, Msg}, {Client, _Reply}, State) ->
    case channel:send_msg(ChannelAtom, Client, Nick, Msg) of
        {error, user_not_joined} ->
            {reply, {error, user_not_joined}, State};
        {error, server_not_reached} ->
            {reply, {error, server_not_reached}, State};
        ok ->
            {reply, ok, State}
    end;
handle_call({join, ChannelAtom}, {Client, _Reply}, State) ->
    % TODO: revert if ChannelAtom == ServerAtom
    case lists:member(ChannelAtom, State#server_state.channels) of
        true ->
            case channel:join(ChannelAtom, Client) of
                % user already joined
                {error, user_already_joined} ->
                    {reply, {error, user_already_joined}, State};
                % join
                ok ->
                    {reply, ok, State}
            end;
        false ->
            % create channel and join
            channel:start(ChannelAtom, Client),
            NewState = State#server_state{channels = [ChannelAtom | State#server_state.channels]},
            {reply, ok, NewState}
    end;
handle_call({leave, ChannelAtom}, {Client, _Reply}, State) ->
    case lists:member(ChannelAtom, State#server_state.channels) of
        true ->
            case channel:leave(ChannelAtom, Client) of
                {error, user_not_joined} ->
                    {reply, {error, user_not_joined}, State};
                ok ->
                    {reply, ok, State}
            end;
        false ->
            {reply, {error, user_not_joined}, State}
    end.

handle_cast(stop, State) ->
    {stop, normal, State}.
