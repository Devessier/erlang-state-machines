-module(custom_state_machines).

-export([start_machine/1, machine_loop/2]).

handle_log_in({state, logged_out}, {event}) ->
    {next_state, logged_in};
handle_log_in({state, _}, {event}) ->
    {noop}.

machine_loop({state, logged_in = FinalState}, Parent) ->
    io:format("Reached final state: ~p~n", [FinalState]),

    Parent ! {state, final};
machine_loop({state, State}, Parent) ->
    io:format("Current state: ~p~n", [State]),

    receive
        {event, log_in = Event} ->
            io:format("Received event: ~p~n", [Event]),

            TransitionResult = handle_log_in({state, State}, {event}),

            case TransitionResult of
                {next_state, NextState} ->
                    machine_loop({state, NextState}, Parent);
                {noop} ->
                    machine_loop({state, State}, Parent)
            end
    end.

start_machine(Parent) ->
    InitialState = logged_out,
    spawn(custom_state_machines, machine_loop, [{state, InitialState}, Parent]).
