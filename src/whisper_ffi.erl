-module(whisper_ffi).

-export([new/1, on/3, subscribe/2, publish/3]).

new(Capacity) ->
    spawn(fun() -> whisper_loop(#{}, Capacity) end).

whisper_loop(Subscribers, Capacity) ->
    receive
        {get_capacity, Caller} ->
            Caller ! {capacity, Capacity},
            whisper_loop(Subscribers, Capacity);
        {register_subscriber, Topic, Id, Buffer, Caller} ->
            TopicSubs = maps:get(Topic, Subscribers, #{}),
            NewTopicSubs = maps:put(Id, Buffer, TopicSubs),
            NewSubscribers = maps:put(Topic, NewTopicSubs, Subscribers),
            Caller ! registered,
            whisper_loop(NewSubscribers, Capacity);
        {register_subscriber, Topic, Id, Buffer} ->
            TopicSubs = maps:get(Topic, Subscribers, #{}),
            NewTopicSubs = maps:put(Id, Buffer, TopicSubs),
            NewSubscribers = maps:put(Topic, NewTopicSubs, Subscribers),
            whisper_loop(NewSubscribers, Capacity);
        {unregister_subscriber, Topic, Id} ->
            case maps:find(Topic, Subscribers) of
                {ok, TopicSubs} ->
                    NewTopicSubs = maps:remove(Id, TopicSubs),
                    NewSubscribers = maps:put(Topic, NewTopicSubs, Subscribers),
                    whisper_loop(NewSubscribers, Capacity);
                error ->
                    whisper_loop(Subscribers, Capacity)
            end;
        {publish, Topic, Msg} ->
            case maps:find(Topic, Subscribers) of
                {ok, TopicSubs} ->
                    maps:foreach(fun(_, Buffer) -> Buffer ! {push, Msg} end, TopicSubs),
                    whisper_loop(Subscribers, Capacity);
                error ->
                    whisper_loop(Subscribers, Capacity)
            end
    end.

on(Whisper, Topic, Listener) ->
    Id = make_ref(),
    Whisper ! {register_subscriber, Topic, Id, self()},

    Loop =
        fun(F) ->
           receive
               {push, Msg} ->
                   Listener(Msg),
                   F(F);
               stop -> ok
           end
        end,

    spawn(fun() -> Loop(Loop) end),

    fun() -> Whisper ! {unregister_subscriber, Topic, Id} end.

subscribe(Whisper, Topic) ->
    Id = make_ref(),
    Whisper ! {get_capacity, self()},
    Capacity =
        receive
            {capacity, Cap} ->
                Cap
        after 1000 ->
            10
        end,

    Buffer = spawn(fun() -> buffer_loop([], Capacity) end),

    Whisper ! {register_subscriber, Topic, Id, Buffer, self()},
    receive
        registered ->
            ok
    after 1000 ->
        ok
    end,

    Receive =
        fun() ->
           Buffer ! {pop, self()},
           receive
               {ok, Msg} -> {ok, Msg};
               empty -> {error, nil}
           after 100 -> {error, nil}
           end
        end,

    Cancel =
        fun() ->
           Whisper ! {unregister_subscriber, Topic, Id},
           Buffer ! stop
        end,

    {Receive, Cancel}.

buffer_loop(Queue, Capacity) ->
    receive
        {push, Msg} ->
            NewQueue = lists:sublist(Queue ++ [Msg], Capacity),
            buffer_loop(NewQueue, Capacity);
        {pop, Caller} ->
            case Queue of
                [] ->
                    Caller ! empty,
                    buffer_loop(Queue, Capacity);
                [H | T] ->
                    Caller ! {ok, H},
                    buffer_loop(T, Capacity)
            end;
        stop ->
            ok
    end.

publish(Whisper, Topic, Msg) ->
    Whisper ! {publish, Topic, Msg}.
