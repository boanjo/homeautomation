%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(portrait).
-include_lib("txrx/include/txrx.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib ("nitrogen_core/include/google_chart.hrl").
-compile(export_all).
-author("Anders Johansson (epkboan@gmail.com)").

main() ->
    #template{file="./priv/templates/portrait.html"}.

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
    [#flash{}].


temp_out() ->
    utils:temp_out(80).
temp_in() ->
    utils:temp_in(80).
rain() ->
    utils:rain(80).
clock() ->
    utils:clock(80).
       

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

event(Events) ->
    wf:remove(mainPanel),
    io:format("What then !~p~n", [Events]),
    wf:flush().

