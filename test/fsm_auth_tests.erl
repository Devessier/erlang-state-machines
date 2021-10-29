-module(fsm_auth_tests).

-include_lib("eunit/include/eunit.hrl").

fsm_auth_initial_state_test() ->
    Machine = fsm_auth:start(),

    Machine ! {state, self()},
    receive
        Message ->
            ?assertMatch({{state, logged_out}, {context, #{}}}, Message)
    end.

fsm_auth_goes_from_logged_out_to_log_in_test() ->
    Machine = fsm_auth:start(),
    Machine ! {event, log_in},

    Machine ! {state, self()},
    receive
        Message ->
            ?assertMatch({{state, logged_in}, {context, #{}}}, Message)
    end.

fsm_auth_goes_back_from_logged_in_test() ->
    Machine = fsm_auth:start(),
    Machine ! {event, log_in},
    Machine ! {event, log_out},

    Machine ! {state, self()},
    receive
        Message ->
            ?assertMatch({{state, logged_out}, {context, #{}}}, Message)
    end.
