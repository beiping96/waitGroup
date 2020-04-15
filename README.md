# waitGroup
A waitGroup waits for a collection of processes to finish. The main process calls `fun new/1` to set the number of processes to wait for. Then each of the processes runs and calls `fun done/1` when finished. At the same time, `fun wait/1` or `fun wait/2` can be used to block until all processes have finished.

Just like `sync.WaitGroup` in Golang.
# Usage
``` erlang
foo() ->
    % Create a waitGroup, set the number of processes to wait for.
    WaitGroup = waitGroup:new(3),

    % Call fun done/1 when Worker has finished. 
    F = fun(Arg) ->
            io:format("~p~n", [Arg]),
            waitGroup:done(WaitGroup)
        end,
    [spawn(F, [A]) || A <- lists:seq(0, 2)],

    % Block until all processes have finished.
    waitGroup:wait(WaitGroup),
    % OR fun wait/2 has the timeout (Milliseconds).
    waitGroup:wait(WaitGroup, 1000),

    allWorkersDone.
```
