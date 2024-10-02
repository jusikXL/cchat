-module(server).
-behaviour(gen_server).

-export([start/1, stop/1]).
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

% callback functions
init(_Args) ->
    InitialState = #server_state{
        channels = []
    },
    {ok, InitialState}.

handle_call({join, ChannelAtom, User}, _Client, State) ->
    case lists:member(ChannelAtom, State#server_state.channels) of
        true ->
            case channel:join(ChannelAtom, User) of
                % user already joined
                {error, user_already_joined} ->
                    {reply, {error, user_already_joined}, State};
                % join
                ok ->
                    {reply, ok, State}
            end;
        false ->
            % create channel and join
            channel:start(ChannelAtom, User),
            NewState = State#server_state{channels = [ChannelAtom | State#server_state.channels]},
            {reply, ok, NewState}
    end.

handle_cast(stop, State) ->
    {stop, normal, State}.
