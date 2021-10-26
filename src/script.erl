-module(script).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_) ->
    Pid = self(),
    Machine = custom_state_machines:start_machine(Pid),
    Machine ! {event, log_in},
    Machine ! {event, log_in},
    receive
        {state, final} ->
            erlang:halt(0)
    end.

%%====================================================================
%% Internal functions
%%====================================================================
