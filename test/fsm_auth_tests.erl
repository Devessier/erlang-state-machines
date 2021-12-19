-module(fsm_auth_tests).

-include_lib("eunit/include/eunit.hrl").

fsm_auth_initial_state_test() ->
    Machine = fsm_auth:start(),

    Machine ! {state, self()},
    receive
        Message ->
            ?assertMatch({{state, logged_out}, {context, #{messages := []}}}, Message)
    end.

fsm_auth_goes_from_logged_out_to_log_in_test() ->
    Machine = fsm_auth:start(),
    Machine ! {event, log_in},

    Machine ! {state, self()},
    receive
        Message ->
            ?assertMatch({{state, logged_in}, {context, #{messages := []}}}, Message)
    end.

fsm_auth_goes_back_from_logged_in_test() ->
    Machine = fsm_auth:start(),
    Machine ! {event, log_in},
    Machine ! {event, log_out},

    Machine ! {state, self()},
    receive
        Message ->
            ?assertMatch({{state, logged_out}, {context, #{messages := []}}}, Message)
    end.

fsm_auth_rejects_messages_in_logged_out_state_test() ->
    Machine = fsm_auth:start(),

    TextMessage = "Hello",
    Machine ! {event, {send_message, TextMessage}},

    Machine ! {state, self()},
    receive
        Message ->
            ?assertMatch({{state, logged_out}, {context, #{messages := []}}}, Message)
    end.

fsm_auth_accepts_messages_in_logged_in_state_test() ->
    Machine = fsm_auth:start(),

    Machine ! {event, log_in},

    TextMessage = "Hello",
    Machine ! {event, {send_message, TextMessage}},

    Machine ! {state, self()},
    receive
        Message ->
            ?assertMatch({{state, logged_in}, {context, #{messages := [TextMessage]}}}, Message)
    end.
