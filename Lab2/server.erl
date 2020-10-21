-module(server).
-export([start/1,stop/1]).

-record(server_State, {
    name, 
    channels
}).

-record(channel_State, {
    name, 
    clients
}).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    genserver:start(ServerAtom, init_Server(ServerAtom), fun handleServer/2).
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID

init_Server(ServerAtom) ->
    #server_State {
        name     = ServerAtom,
        channels = []
}.

init_Channel(ChannelAtom, Client) ->
    #channel_State {
        name = ChannelAtom,
        clients = [Client]
}.

handleServer(State, {join, Channel, Client}) ->
    ChannelAtom = list_to_atom(Channel),
    case lists:member(ChannelAtom, State#server_State.channels) of
        true -> 
            case genserver:request(ChannelAtom, {join, Client}) of
                ok -> {reply, ok, State};
                F -> {reply, F, State} 
            end;
        false -> 
            genserver:start(ChannelAtom, init_Channel(Channel, Client), fun handleChannel/2),
                {reply, ok, State#server_State{channels = [ChannelAtom | State#server_State.channels]}}
end;

handleServer(State, {stop}) ->
    [genserver:stop(Channel) || Channel <- State#server_State.channels],
    {reply, ok, State}.

handleChannel(State, {join, Client}) ->
    case lists:member(Client, State#channel_State.clients) of 
        true -> {reply, {error, user_already_joined, "User already in channel"}, State};
        false -> {reply, ok, State#channel_State{clients = [Client | State#channel_State.clients]}}
    end;

handleChannel(State, {leave, Client}) ->
    case lists:member(Client, State#channel_State.clients) of 
        true -> {reply, left, State#channel_State{clients = lists:delete(Client, State#channel_State.clients)}};
        false -> {reply, user_not_joined, State}
    end;

handleChannel(State, {message_send, Client, From, Message}) ->
    case lists:member(Client, State#channel_State.clients) of 
        true -> spawn(fun() -> 
            [genserver:request(Receivers, {message_receive, State#channel_State.name, From, Message}) || 
            Receivers <- State#channel_State.clients, Receivers =/= Client] end),
            {reply, ok, State};
        false -> {reply, {error, user_not_joined, "User not in channel"}, State}
    end.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:request(ServerAtom, {stop}),
    genserver:stop(ServerAtom).
    % Return ok
