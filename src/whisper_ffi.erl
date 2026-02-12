-module(whisper_ffi).

-export([register/2, on/2, subscribe/1, publish/2, close/1]).

register(Topic, Capacity) ->
    whisper_registry:ensure_started(),
    whisper_registry:register(Topic, Capacity).

on(Topic, Listener) ->
    whisper_registry:ensure_started(),
    case whisper_registry:get_topic_pid(Topic) of
        undefined ->
            fun() -> nil end;
        Pid ->
            whisper_topic:add_listener(Pid, Listener)
    end.

subscribe(Topic) ->
    whisper_registry:ensure_started(),
    case whisper_registry:get_topic_pid(Topic) of
        undefined ->
            ClosedFn = fun() -> {closed, topic_is_not_registered} end,
            {ClosedFn, fun() -> nil end};
        Pid ->
            whisper_topic:subscribe(Pid)
    end.

publish(Topic, Message) ->
    whisper_registry:ensure_started(),
    case whisper_registry:get_topic_pid(Topic) of
        undefined ->
            nil;
        Pid ->
            whisper_topic:publish(Pid, Message)
    end.

close(Topic) ->
    whisper_registry:ensure_started(),
    whisper_registry:close_topic(Topic).
