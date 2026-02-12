-module(whisper_registry).

-behaviour(gen_server).

-export([start_link/0, ensure_started/0, register/2, get_topic_pid/1, close_topic/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

ensure_started() ->
    case whereis(?MODULE) of
        undefined ->
            case start_link() of
                {ok, _Pid} ->
                    ok;
                {error, {already_started, _Pid}} ->
                    ok;
                Error ->
                    Error
            end;
        _Pid ->
            ok
    end.

register(Topic, Capacity) ->
    gen_server:call(?MODULE, {register, Topic, Capacity}).

get_topic_pid(Topic) ->
    case ets:lookup(whisper_registry, Topic) of
        [{_, Pid}] ->
            Pid;
        [] ->
            undefined
    end.

close_topic(Topic) ->
    gen_server:call(?MODULE, {close_topic, Topic}).

init([]) ->
    ets:new(whisper_registry, [set, public, named_table]),
    {ok, #{}}.

handle_call({register, Topic, Capacity}, _From, State) ->
    case ets:lookup(whisper_registry, Topic) of
        [] ->
            {ok, Pid} = whisper_topic:start_link(Topic, Capacity),
            ets:insert(whisper_registry, {Topic, Pid}),
            monitor(process, Pid),
            Whisper = {whisper, Topic, Capacity},
            {reply, {ok, Whisper}, State};
        _ ->
            {reply, {error, already_registered}, State}
    end;
handle_call({close_topic, Topic}, _From, State) ->
    case get_topic_pid(Topic) of
        undefined ->
            {reply, ok, State};
        Pid ->
            gen_server:stop(Pid, normal, 5000),
            ets:delete(whisper_registry, Topic),
            {reply, ok, State}
    end;
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    % Clean up if topic process crashes
    ets:match_delete(whisper_registry, {'_', Pid}),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
