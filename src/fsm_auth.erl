-module(fsm_auth).

-export([start/0, loop/0]).

logged_in(log_out, Context) ->
    {{next_state, logged_out}, Context};
logged_in(_Event, _Context) ->
    keep_state_and_context.

logged_out(log_in, Context) ->
    {{next_state, logged_in}, Context};
logged_out(_Event, _Context) ->
    keep_state_and_context.

call_state_handler(State, Context, Event) ->
    case State of
        logged_in ->
            logged_in(Event, Context);
        logged_out ->
            logged_out(Event, Context)
    end.

loop() ->
    InitialState = logged_out,
    InitialContext = #{},

    loop(InitialState, InitialContext, void).

loop(State, Context, OldState) ->
    if State =/= OldState ->
           io:format("New state: ~p with context: ~p~n", [State, Context]);
       true ->
           void
    end,

    receive
        {event, Event} ->
            HandlerReturn = call_state_handler(State, Context, Event),
            case HandlerReturn of
                keep_state_and_context ->
                    loop(State, Context, State);
                {keep_state, NewContext} ->
                    loop(State, NewContext, State);
                {{next_state, NewState}, NewContext} ->
                    loop(NewState, NewContext, State)
            end;
        {state, From} ->
            From ! {{state, State}, {context, Context}},
            loop(State, Context, State)
    end.

start() ->
    spawn(fsm_auth, loop, []).
