-module(homeautomation_server).

-behaviour(gen_server).

-export([start_link/0, say_hello/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, start_timers/2]).

start_link() ->
    
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->    
    case application:get_env(homeautomation, device) of
	{ok, List} ->
	    
	    check_devices_for_timers(List);
	_ ->
	    ok
    end,
    {ok, []}.

check_devices_for_timers([]) ->
    ok;
check_devices_for_timers([Device|Devices]) ->
    {Id, _Name, List} = Device,
    start_timers(Id, List),
    check_devices_for_timers(Devices).

start_timers(_Id, []) ->
    ok;

start_timers(Id, [{Cmd, Time}|Events]) ->    
    Msg = {Id, Cmd, Time, permanent},
    gen_server:cast(?MODULE, {start_timer, Msg}),
    start_timers(Id, Events).

say_hello() ->
    
    gen_server:call(?MODULE, hello).



date_diff(D1, D2) ->    
    calendar:datetime_to_gregorian_seconds(D1) -
        calendar:datetime_to_gregorian_seconds(D2).

next_day({Date, Time}) ->
    Days = calendar:date_to_gregorian_days(Date),
    NextDate = calendar:gregorian_days_to_date(Days+1),
    {NextDate, Time}.

%% callbacks
handle_call(hello, _From, State) ->    
    io:format("Hello from server!~n", []),
    {reply, ok, State};


handle_call(_Request, _From, State) ->
    io:format("1 Timeout!~n", []),
    Reply = ok,
    {reply, Reply, State}.

handle_cast({start_timer, Msg}, State) -> 
        {_, _, EventTime, _} = Msg,
    Now = calendar:local_time(),
    {NowDate, _} = Now,
    TimeOut = {NowDate, EventTime},
    Diff = date_diff(TimeOut, Now),
    
    %% We use 1 instead of 0 to make sure we don't start new timer 
    %% for todays timeout
    Secs = if Diff > 1  -> Diff;
	      Diff =< 1 -> date_diff(next_day(TimeOut), Now)
	   end,
    
    timer:send_after(Secs*1000, self(), {timeout, Msg}),
    io:format("Starting timer with tmo value ~p secs~n", [Secs]),
    {noreply, State};

handle_cast(_Msg, State) ->
    io:format("2 Timeout!~n", []),
    {noreply, State}.

handle_info({timeout, Msg}, State) ->    
    io:format("Timeout Msg=~p Time=~p ~n", [Msg, calendar:local_time()]),
    {Id, Cmd, _Time, Type} = Msg,
    tellstick_server:device(Id, Cmd),

    case Type of
	permanent ->
	    gen_server:cast(?MODULE, {start_timer, Msg});
	_Transient -> ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    io:format("3 Timeout!~n", []),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
