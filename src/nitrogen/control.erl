%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(control).
-include_lib("txrx/include/txrx.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).
-author("Anders Johansson (epkboan@gmail.com)").

main() -> #template{file="./priv/templates/control.html"}.

title() ->     
    {ok, Title} = application:get_env(homeautomation, title),
    Title.

footer() ->
    {{Year,_Month,_Day},{_Hour,_Minutes,_Seconds}} = erlang:localtime(),
    "&copy; " ++ integer_to_list(Year) ++ " - Anders Johansson".


xmlencode([], Acc) -> Acc; 
xmlencode([$<|T], Acc) -> xmlencode(T, Acc ++ "&lt;"); % euro symbol
xmlencode([$å|T], Acc) -> xmlencode(T, Acc ++ "&aring;");
xmlencode([$ä|T], Acc) -> xmlencode(T, Acc ++ "&auml;");
xmlencode([$ö|T], Acc) -> xmlencode(T, Acc ++ "&ouml;");
xmlencode([$Å|T], Acc) -> xmlencode(T, Acc ++ "&Aring;");
xmlencode([$Ä|T], Acc) -> xmlencode(T, Acc ++ "&Auml;");
xmlencode([$Ö|T], Acc) -> xmlencode(T, Acc ++ "&Ouml;");
xmlencode([226,130,172|T], Acc) -> xmlencode(T, Acc ++ "&#8364;");
xmlencode([OneChar|T], Acc) -> xmlencode(T, lists:flatten([Acc,OneChar])). 


map_state(State) ->
   case State of
       1 ->
	   "on";
       _ ->
	   "off"
   end.


add_alarm_cell(_Id, []) ->
    [];
add_alarm_cell(Id, _AlarmList) ->
    Element = #tablecell { 
		 align=right,
		 valign=bottom,
		 body =[
			#button { 
			   id=list_to_atom("button_" ++ integer_to_list(Id)),
			   click=#toggle{target=list_to_atom("alarm_menu" ++ integer_to_list(Id))},
			%%			   postback={click, list_to_atom("button_" ++ integer_to_list(Id)), Id} ,
			   data_fields=[{icon, 'arrow-d'},
			   		{mini, true},
			   		{inline, true},
			   		{theme, c},
			   		{iconpos, notext}]
			  }
		       ]},
    Element.



add_alarm_menu(_Id, []) ->
    [];
add_alarm_menu(Id, AlarmList) ->
    Element = 	#mobile_list{
		   id=list_to_atom("alarm_menu" ++ integer_to_list(Id)),
		   theme=a,
		   inset=true,
		   style="display:none",
		   body=[#mobile_list_divider{class=c, text="Timers"}] ++ create_alarms(AlarmList, [])
		  },
    Element.

create_alarms([], Acc) ->
    Acc;

create_alarms([Head|Tail], Acc) ->

    {Cmd, {H,M,S}} = Head,
    Element = #mobile_listitem{
		 theme=c,
		 body=   #mobile_grid { 
			    columns=2,
			    blocks=[
				    #mobile_grid_block{ text=Cmd },
				    #mobile_grid_block{ text=io_lib:format("at ~p:~p:~p", [H,M,S])}]
			   }
		},
    
    create_alarms(Tail, [Element|Acc]).

create_device([], Acc) ->
    Acc;

create_device([Head|Tail], Acc) ->

    {Id, Address, Unit, Name, AlarmList} = Head,   
    txrx_server:set_device_info(Address, Unit, Id),    

    Device = txrx_server:get_device(Id),

    NewName = xmlencode(Name, []),

    ListElement = [#label { text=NewName, html_encode=false },
		   #table { rows=[
				  #tablerow { 
				     cells=[
					    #tablecell { 
					       body =[
						      #mobile_toggle{
							 on_text="on",
							 off_text="off",

							 selected=map_state(Device#device.state),
							 postback=Id,
							 id=list_to_atom("device_" ++ integer_to_list(Id)),
							 width=100
							}]},
					    add_alarm_cell(Id, AlarmList),
					    add_alarm_menu(Id, AlarmList)
					    
					   ]
				    }
				 ]
			  },
		   
		   #hr{}],
    create_device(Tail, [ListElement|Acc]).


create_temperature([], Acc) ->
    Acc;

create_temperature([Head|Tail], Acc) ->

    {_, {Id, Name}} = Head,
    T = txrx_server:get_sensor(Id),

    case T of
	not_found ->
	    create_temperature(Tail, Acc);
	_ ->
	    Diff = timer:now_diff(erlang:now(), T#sensor.last_update_time)/1000000,
	    
	    ListElement = [#mobile_listitem{
			      theme=c,
			      body=   #mobile_grid { 
					 columns=3,
					 blocks=[
						 #mobile_grid_block{ text=Name },
						 #mobile_grid_block{ body=[#label{text=io_lib:format("~.1f&deg;C",[T#sensor.value]), html_encode=false}] },
						 #mobile_grid_block{ text=io_lib:format("Updated ~.1f sec ago", [Diff])}
						]
					}
			     }],
	    
	    create_temperature(Tail, [ListElement|Acc])
    end.

			 
create_humidity([], Acc) ->
    Acc;

create_humidity([Head|Tail], Acc) ->

    {_, {Id, Name}} = Head,
    H = txrx_server:get_sensor(Id),
    
    case H of
	not_found ->
	    create_humidity(Tail, Acc);
	_ ->
	    
	    Diff = timer:now_diff(erlang:now(), H#sensor.last_update_time)/1000000,
	    
	    ListElement = [#mobile_listitem{
			      theme=c,
			      body=   #mobile_grid { 
					 columns=3,
					 blocks=[
						 #mobile_grid_block{ text=Name },
						 #mobile_grid_block{ text=lists:flatten(integer_to_list(H#sensor.value) ++ "%")},
						 #mobile_grid_block{ text=io_lib:format("Updated ~.1f sec ago", [Diff])}
						]
					}
			     }],
	    
	    create_humidity(Tail, [ListElement|Acc])
    end.
			 
create_rain([], Acc) ->
    Acc;

create_rain([Head|Tail], Acc) ->

    {_Atom, {Id, Name}} = Head,
    H = txrx_server:get_sensor(Id),
    
    case H of
	not_found ->
	    create_rain(Tail, Acc);
	_ ->
	    
	    Diff = timer:now_diff(erlang:now(), H#sensor.last_update_time)/1000000, 
	    ListElement = [#mobile_listitem{
			      theme=c,
			      body=   #mobile_grid { 
					 columns=3,
					 blocks=[
						 #mobile_grid_block{ text=Name },
						 #mobile_grid_block{ text=io_lib:format("~.1f mm",[H#sensor.value])},
						 #mobile_grid_block{ text=io_lib:format("Updated ~.1f sec ago", [Diff])}
						]
					}
			     }],
	    
	    create_rain(Tail, [ListElement|Acc])
    end.
create_wind([], Acc) ->
    Acc;

create_wind([Head|Tail], Acc) ->

    {_Atom, {Id, Name}} = Head,
    H = txrx_server:get_sensor(Id),
    
    case H of
	not_found ->
	    create_wind(Tail, Acc);
	_ ->
	    
	    Diff = timer:now_diff(erlang:now(), H#sensor.last_update_time)/1000000, 
	    ListElement = [#mobile_listitem{
			      theme=c,
			      body=   #mobile_grid { 
					 columns=3,
					 blocks=[
						 #mobile_grid_block{ text=Name },
						 case is_float(H#sensor.value) of
						     true ->

							 #mobile_grid_block{ text=io_lib:format("~.1f m/s",[H#sensor.value])};
						     _ ->
							 #mobile_grid_block{ text=io_lib:format("~p ",[H#sensor.value])}
						 end,
						 #mobile_grid_block{ text=io_lib:format("Updated ~.1f sec ago", [Diff])}
						]
					}
			     }],
	    
	    create_wind(Tail, [ListElement|Acc])
    end.
			 

			 

body() ->         
    {ok, WantedDeviceList} = application:get_env(homeautomation, device),
    {ok, WantedTemperatureSensorList} = application:get_env(homeautomation, temperature),
    {ok, WantedHumiditySensorList} = application:get_env(homeautomation, humidity),
    {ok, WantedRainSensorList} = application:get_env(homeautomation, rain),
    {ok, WantedWindSensorList} = application:get_env(homeautomation, wind),

    Elements = create_device(WantedDeviceList, []),
    
    TempList = [#mobile_list{
		   id=menu,
		   theme=a,
		   inset=true,
		   %%    		style="display:none",
		   body=[#mobile_list_divider{class=c, text="Temperatur"}] ++ 
		       create_temperature(WantedTemperatureSensorList, [])
		   
		   
		  }
	       ],

    HumList = [#mobile_list{
		  id=menu,
		  theme=a,
		  inset=true,
		  %%    		style="display:none",
		  body=[#mobile_list_divider{class=c, text="Luftfuktighet"}] ++ 
		      create_humidity(WantedHumiditySensorList, [])
		 }
	      ],
    RainList = [#mobile_list{
		   id=menu,
		   theme=a,
		   inset=true,
		   %%    		style="display:none",
		   body=[#mobile_list_divider{class=c, text="Regn"}] ++ 
		       create_rain(WantedRainSensorList, [])
		 }
	      ],
    WindList = [#mobile_list{
		   id=menu,
		   theme=a,
		   inset=true,
		   %%    		style="display:none",
		   body=[#mobile_list_divider{class=c, text="Vind"}] ++ 
		       create_wind(WantedWindSensorList, [])
		 }
	      ],
    wf:f(Elements ++ TempList ++ HumList ++ RainList ++ WindList).

event(Id) 
  when is_integer(Id)->
    Device = "device_" ++ integer_to_list(Id),
    IdAtom = list_to_atom(Device),
    SendCmd = wf:q(IdAtom),
    txrx_server:device(Id, list_to_atom(SendCmd));

event({click, Button, Id}) ->
    ShowMenu = wf:q(Button),
    
    Menu = wf:q(list_to_atom("alarm_menu" ++ integer_to_list(Id))),
    error_logger:info_msg("ShowMenu ~p for button ~p menu ~p~n", [ShowMenu, Button, Menu]),
    case ShowMenu of
        "on" -> wf:wire(list_to_atom("alarm_menu" ++ integer_to_list(Id)),#slide_down{});
        _Other -> wf:wire(list_to_atom("alarm_menu" ++ integer_to_list(Id)),#slide_up{})
    end;

event(Other) ->    
    error_logger:info_msg("Other ~p~n", [Other]).
