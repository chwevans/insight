# insight
Metrics collection and timeliness guarantees in erlang

## Examples
Basic usage:
```
1> insight:measure(fun() -> 3 + 4 end, #{label => <<"foo">>}).
7
2> insight:measure({math, pow, [2, 3]}, #{}).
8.0
```

Labels are optional for an MFA call, but required for anonymous functions.
Labels are used as names for metrics collected by insight.

## Metrics
By default, insight will record metrics to insight_metrics. Metrics can be
examined using all/2, mean/2, and median/2.

```
1> insight_metrics:median(timing, <<"math:pow">>).
5
```

insight_metrics is not designed to be used in a production setting (it claims memory without cleaning up)
and a custom metrics collection solution should be used instead.
```
1> insight:measure({math, pow, [2, 3]}, #{metric_fun => {custom_solution, record}}),
8.0
```

metric_fun should refer to a 3-arity function:
```
-spec record(completed | timing | reductions, binary(), pos_integer()) -> ok.
```

Currently the supported metrics that are collected are:
* completed: Always called with a value of 1 and refers to the number of times the measured functions are called.
* timing: The amount of time the measured function takes to complete in microseconds
* reductions: The amount of reductions the function takes

By specifying `metrics` in the options map, you can stop collecting undesired metrics.
`metrics` defaults to `[completed, timing, reductions]`. Currently specifying less metrics
in the flag will not save the computation spent to record them, but will prevent the metrics
function from being called.

## Timelines guarantees
Insight provides a variety of ways to ensure a function call completes in a desired interval.

```
1> insight:measure(fun() -> timer:sleep(5), 5 end, #{label => <<"will_succeed">>, timeout => 100}).
5
2> insight:measure(fun() -> timer:sleep(5), 5 end, #{label => <<"will_fail">>, timeout => 1}).
{error,timeout}
```

Timeout will return to the caller in the desired timeframe. If you want to free resources
associated with the function when timeout is exceeded, you can use the `brutal_kill` option.
This will exit the executing function before it has the chance to finish. `brutal_kill` should
be used when you want to quit computation when you timeout. Without `brutal_kill` computation
will be continued regardless of the response to the caller of insight:measure/2.
```
1> insight:measure(fun() -> timer:sleep(5), 7 end, #{label => <<"will_fail">>, timeout => 1, brutal_kill => true}).
{error,timeout}
```

If its desired to spawn off a job but still collect metrics on it, `spawn` can be used.
```
1> insight:measure(fun() -> timer:sleep(5), 7 end, #{label => <<"will_fail">>, timeout => 1, brutal_kill => true, spawn => true}).
ok
```
Note that when `spawn` is `true`, `ok` will always be returned despite what actually happened.
`brutal_kill` and `timeout` will also be respected.

