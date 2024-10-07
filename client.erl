-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    % atom of the GUI process
    gui,
    % nick/username of the client
    nick,
    % atom of the chat server
    server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    % Sends message to the server and forwards the response
    case server:join(St#client_st.server, list_to_atom(Channel)) of
        ok ->
            {reply, ok, St};
        {error, user_already_joined} ->
            {reply, {error, user_already_joined, "User already joined"}, St};
        {error, server_not_reached} ->
            {reply, {error, server_not_reached, "Server not reached"}, St}
    end;
% Leave channel
handle(St, {leave, Channel}) ->
    % Sends message to the specified channel and forwards the response
    case channel:leave(list_to_atom(Channel)) of
        ok ->
            {reply, ok, St};
        {error, user_not_joined} ->
            {reply, {error, user_not_joined, "User not joined"}, St};
        {error, server_not_reached} ->
            {reply, {error, server_not_reached, "Server not reached"}, St}
    end;
% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    % Sends message to the specified channel and forwards the response
    case channel:send_msg(list_to_atom(Channel), St#client_st.nick, Msg) of
        ok ->
            {reply, ok, St};
        {error, user_not_joined} ->
            {reply, {error, user_not_joined, "User not joined"}, St};
        {error, server_not_reached} ->
            {reply, {error, server_not_reached, "Server not reached"}, St}
    end;
% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}};
% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St};
% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick ++ "> " ++ Msg}),
    {reply, ok, St};
% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St};
% Catch-all for any unhandled requests
handle(St, _Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St}.
