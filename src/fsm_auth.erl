-module(fsm_auth).

-export([start/0, loop/0]).

logged_in({send_message, Message}, #{messages := Messages} = Context) ->
    {keep_state, Context#{messages := [Message | Messages]}};
logged_in(log_out, _Context) ->
    {next_state, logged_out};
logged_in(_Event, Context) ->
    {keep_state, Context}.

logged_out(log_in, _Context) ->
    {next_state, logged_in};
logged_out(_Event, Context) ->
    {keep_state, Context}.

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

    loop(InitialState, InitialContext).

loop(State, Context) ->
    io:format("State: ~p with context: ~p~n", [State, Context]),

    receive
        {event, Event} ->
            HandlerReturn = call_state_handler(State, Context, Event),
            case HandlerReturn of
                {keep_state, NewContext} ->
                    loop(State, NewContext);
                {next_state, NewState} ->
                    loop(NewState, Context)
            end;
        {state, From} ->
            From ! {{state, State}, {context, Context}},
            loop(State, Context)
    end.

start() ->
    spawn(fsm_auth, loop, []).
