%%% @author <beiping96@gmail.com>
%%% @version 20170830 first release
%%% @doc
%%%     A waitGroup waits for a collection of processes to finish.
%%%     Just like WaitGroup in Golang's sync.
%%% 
%%%     Tips: fun new/1 And (fun wait/1 or fun wait/2)
%%%           MUST be called in one process.
-module(waitGroup).

-type waitGroup() :: {pid(), reference()}.
-type counter() :: non_neg_integer().
-type expire_milliseconds() :: non_neg_integer() | infinity.

%% @doc API export
-export([new/1, wait/1, wait/2]).
-export([done/1]).

%% @hidden private function
-export([waitGroup/2]).

%% @doc create a waitGroup
-spec new(counter()) -> waitGroup().
new(Counter) ->
    case is_integer(Counter) andalso Counter >= 0 of
        true ->
            WaitGroup = spawn_monitor(?MODULE, waitGroup, [self(), Counter]),
            erlang:put(WaitGroup, true),
            WaitGroup;
        false ->
            erlang:error(badarg, [Counter])
    end.

%% @doc Counter - 1
-spec done(waitGroup()) -> any().
done({Pid, _Ref}) ->
    Pid ! {self(), done}.

%% @doc return When Counter == 0
-spec wait(waitGroup()) -> any().
wait(WaitGroup) ->
    wait(WaitGroup, infinity).
-spec wait(waitGroup(), expire_milliseconds()) -> any().
wait({Pid, Ref}, ExpireMilliSeconds) ->
    case ExpireMilliSeconds =:= infinity orelse (is_integer(ExpireMilliSeconds) andalso ExpireMilliSeconds >= 0) of
        true -> ok;
        false -> erlang:error(badarg, [waitGroup, ExpireMilliSeconds])
    end,
    case is_process_alive(Pid) of
        true -> Expire = ExpireMilliSeconds;
        false -> Expire = 0
    end,
    receive
        {'DOWN', Ref, process, Pid, _Info} ->
            ok
    after Expire ->
        ok
    end.

%% @hidden waitGroup
-spec waitGroup(pid(), counter()) -> any().
waitGroup(_MainPid, 0) ->
    ok;
waitGroup(MainPid, Counter) when is_integer(Counter) andalso Counter > 0 ->
    receive
        {MainPid, done} ->
            waitGroup(MainPid, Counter - 1)
    after 1000 ->
        case is_process_alive(MainPid) of
            true -> waitGroup(MainPid, Counter);
            false -> ok
        end
    end.

