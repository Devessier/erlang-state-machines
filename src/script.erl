-module(script).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_) ->
    Machine = fsm:start(),

    {{state, logged_out}, {context, #{messages := []}}} = fsm:get_state(Machine),
    io:format("Currently logged out with empty messages list~n"),

    fsm:send_event(Machine, log_out),

    {{state, logged_out}, _} = fsm:get_state(Machine),
    io:format("Still logged out after a log_out event~n"),

    fsm:send_event(Machine, log_in),
    fsm:send_event(Machine, log_in),

    {{state, State}, {context, Context}} = fsm:get_state(Machine),
    io:format("state and context: ~p|~p~n", [State, Context]),

    fsm:send_event(Machine, {send_message, "This is my message"}),

    {{state, StateAfterMessageSending}, {context, ContextAfterMessageSending}} =
        fsm:get_state(Machine),
    io:format("state and context: ~p|~p~n",
              [StateAfterMessageSending, ContextAfterMessageSending]),

    void.

%%====================================================================
%% Internal functions
%%====================================================================
