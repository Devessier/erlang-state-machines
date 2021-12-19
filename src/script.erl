-module(script).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_) ->
    Machine = fsm_auth:start(),

    Machine ! {state, self()},
    receive
        {{state, logged_out}, {context, #{messages := []}}} ->
            void
    end,
    io:format("Currently logged out with empty messages list~n"),

    Machine ! {event, log_out},

    Machine ! {state, self()},
    receive
        {{state, logged_out}, _} ->
            void
    end,
    io:format("Still logged out after a log_out event~n"),

    Machine ! {event, log_in},
    Machine ! {event, log_in},

    Machine ! {state, self()},
    receive
        {{state, State}, {context, Context}} ->
            void
    end,
    io:format("state and context: ~p|~p~n", [State, Context]),

    Machine ! {event, {send_message, "This is my message"}},

    Machine ! {state, self()},
    receive
        {{state, StateAfterMessageSending}, {context, ContextAfterMessageSending}} ->
            void
    end,
    io:format("state and context: ~p|~p~n",
              [StateAfterMessageSending, ContextAfterMessageSending]),

    void.

%%====================================================================
%% Internal functions
%%====================================================================
