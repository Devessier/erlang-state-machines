-module(fsm).

-export([start/0, loop/0, get_state/1, send_event/2]).

logged_in({send_message, Message}, #{messages := Messages} = Context) ->
    {keep_state, Context#{messages := [Message | Messages]}};
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
    InitialContext = #{messages => []},

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
    spawn(fsm, loop, []).

get_state(Machine) ->
    Machine ! {state, self()},
    receive
        {{state, _State}, {context, _Context}} = MachineState ->
            MachineState
    end.

send_event(Machine, Event) ->
    Machine ! {event, Event}.
