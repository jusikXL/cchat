-module(channel).
-behaviour(gen_server).

-export([start/2, stop/1, join/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(channel_state, {
    users
}).

% interface functions
start(ChannelAtom, User) ->
    catch (unregister(ChannelAtom)),
    gen_server:start({local, ChannelAtom}, server, User, []).

stop(ChannelAtom) ->
    gen_server:cast(ChannelAtom, stop).

join(ChannelAtom, User) ->
    gen_server:call(ChannelAtom, {join, User}).

% callback functions
init(User) ->
    InitialState = #channel_state{
        users = [User]
    },
    {ok, InitialState}.

handle_call({join, User}, _Server, State) ->
    case lists:member(User, State#channel_state.users) of
        true ->
            % already joined
            {reply, {error, user_already_joined}, State};
        false ->
            % join
            NewState = #channel_state{
                users = [User | State#channel_state.users]
            },
            {reply, ok, NewState}
    end.

handle_cast(stop, State) ->
    {stop, normal, State}.
