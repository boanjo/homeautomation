%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(landscape).
-include_lib("txrx/include/txrx.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib ("nitrogen_core/include/google_chart.hrl").
-compile(export_all).
-author("Anders Johansson (epkboan@gmail.com)").

main() ->
    #template{file="./priv/templates/landscape.html"}.

title() ->

    {ok, Title} = application:get_env(homeautomation, title),
    utils:xmlencode(Title, []).

footer() ->
    {{Year,_Month,_Day},{_Hour,_Minutes,_Seconds}} = erlang:localtime(),
    "&copy; " ++ integer_to_list(Year) ++ " - Anders Johansson".

forecast() ->

    {ok, {{latitude, Lat}, {longitude, Lon}}} = application:get_env(homeautomation, position),

    "http://forecast.io/embed/#&lat=" ++ Lat ++ "&lon=" ++ Lon ++ 
	"&name=" ++ re:replace(title(), " ", "%20", [{return,list}]) ++ "&lang=sv&units=si&color=#00aaff".


body() ->


    [#flash{}
     ].

temp_out() ->
    utils:temp_out(100).
temp_in() ->
    utils:temp_in(100).
rain() ->
    utils:rain_with_clock(100).
wind() ->
    utils:wind(100).

background_update(ControlID, Count) ->
    
    %% {AvgVal, _AvgMin, _AvgMax, _AvgUpdated} = utils:get_sensor_values(avg, wind, "m/s"), 
    %% {DirVal, _DirMin, _DirMax, _DirUpdated} = utils:get_sensor_values(dir, wind, ""), 

    % Sleep for a second, then update
    timer:sleep(1000),
    
    % Update the control.
%%    wf:update(ControlID, AvgVal ++ " " ++ dir_2_arrow(DirVal)),
    
    % wf:comet_flush() is only needed because we are looping. Otherwise,
    % we could just let the function complete.
    wf:flush(),
    
  % Loop. This process will automatically be killed once the page stops
  % requesting the output that it generates.
  %
  % Using ?MODULE before the function call will ensure that this process
  % survives code reloads.
    ?MODULE:background_update(ControlID, Count + 1.0).

get_last_week() ->

    {ok, List1} = application:get_env(homeautomation, temperature),
    {TempId, _Text1} = proplists:get_value(out, List1),
    Temp = mysql_server:get_min_max_last_week(TempId),

    {ok, List2} = application:get_env(homeautomation, rain),
    {RainId, _Text2} = proplists:get_value(out, List2),
    Rain = mysql_server:get_min_max_last_week(RainId),
    Ret = utils:acc_text(Temp, Rain, []),
    Ret.

event(Event) when is_atom(Event) ->

    wf:flash(utils:info(Event));

event(my_click_event) ->  
    error_logger:info_msg("Click ~p~n", [my]);

event(Other) -> 
    wf:flash("HEJJJJ"),
    error_logger:info_msg("Other ~p~n", [Other]).
