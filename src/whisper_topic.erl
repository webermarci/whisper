-module(whisper_topic).

-behaviour(gen_server).

-export([start_link/2, add_listener/2, subscribe/1, publish/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link(Topic, Capacity) ->
    gen_server:start_link(?MODULE, {Topic, Capacity}, []).

add_listener(Pid, Listener) ->
    ListenerId = make_ref(),
    gen_server:cast(Pid, {add_listener, ListenerId, Listener}),
    fun() -> gen_server:cast(Pid, {remove_listener, ListenerId}) end.

subscribe(Pid) ->
    SubscriberId = make_ref(),
    Capacity = gen_server:call(Pid, get_capacity),
    Buffer = whisper_buffer:new_buffer(Capacity),
    gen_server:cast(Pid, {add_subscriber, SubscriberId, Buffer}),

    ReceiveFn =
        fun() ->
           try gen_server:call(Pid, {receive_message, SubscriberId}, 1000) of
               Result -> Result
           catch
               exit:{noproc, _} -> {closed, topic_is_not_registered};
               exit:{timeout, _} -> {closed, topic_is_not_registered};
               _:_ -> {closed, topic_is_not_registered}
           end
        end,

    CancelFn =
        fun() ->
           catch gen_server:cast(Pid, {remove_subscriber, SubscriberId}),
           ok
        end,

    {ReceiveFn, CancelFn}.

publish(Pid, Message) ->
    gen_server:cast(Pid, {publish, Message}).

init({Topic, Capacity}) ->
    {ok,
     #{topic => Topic,
       capacity => Capacity,
       listeners => #{},
       subscribers => #{}}}.

handle_call(get_capacity, _From, State) ->
    {reply, maps:get(capacity, State), State};
handle_call({receive_message, SubscriberId}, _From, State) ->
    Subscribers = maps:get(subscribers, State),
    case maps:get(SubscriberId, Subscribers, undefined) of
        undefined ->
            {reply, {closed, subscription_cancelled}, State};
        Buffer ->
            case whisper_buffer:pop(Buffer) of
                {{ok, Message}, NewBuffer} ->
                    NewSubscribers = maps:put(SubscriberId, NewBuffer, Subscribers),
                    {reply, {message, Message}, State#{subscribers => NewSubscribers}};
                {{error, nil}, _} ->
                    {reply, empty, State}
            end
    end;
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({add_listener, ListenerId, Listener}, State) ->
    Listeners = maps:get(listeners, State),
    NewListeners = maps:put(ListenerId, Listener, Listeners),
    {noreply, State#{listeners => NewListeners}};
handle_cast({remove_listener, ListenerId}, State) ->
    Listeners = maps:get(listeners, State),
    NewListeners = maps:remove(ListenerId, Listeners),
    {noreply, State#{listeners => NewListeners}};
handle_cast({add_subscriber, SubscriberId, Buffer}, State) ->
    Subscribers = maps:get(subscribers, State),
    NewSubscribers = maps:put(SubscriberId, Buffer, Subscribers),
    {noreply, State#{subscribers => NewSubscribers}};
handle_cast({remove_subscriber, SubscriberId}, State) ->
    Subscribers = maps:get(subscribers, State),
    NewSubscribers = maps:remove(SubscriberId, Subscribers),
    {noreply, State#{subscribers => NewSubscribers}};
handle_cast({publish, Message}, State) ->
    % Call all listeners
    Listeners = maps:get(listeners, State),
    maps:foreach(fun(_, Listener) -> Listener(Message) end, Listeners),

    % Push to all subscriber buffers
    Subscribers = maps:get(subscribers, State),
    NewSubscribers =
        maps:map(fun(_, Buffer) -> whisper_buffer:push(Buffer, Message) end, Subscribers),

    {noreply, State#{subscribers => NewSubscribers}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
