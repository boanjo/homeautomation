%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(mobile).
-include_lib("tellstick/include/tellstick.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).
-author("Anders Johansson (epkboan@gmail.com)").

main() -> #template{file="./priv/templates/mobile.html"}.

title() -> "Oxelgatan 7".

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

    {Id, Name, AlarmList} = Head,   
    Device = tellstick_server:get_device(Id),

    
    
    ListElement = [#label { text=Name, html_encode=true },
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

    {Id, Name} = Head,
    T = tellstick_server:get_temperature(Id),

    case T of
	not_found ->
	    create_temperature(Tail, Acc);
	_ ->
	    Diff = timer:now_diff(erlang:now(), T#temperature.last_update_time)/1000000,
	    
	    ListElement = [#mobile_listitem{
			      theme=c,
			      body=   #mobile_grid { 
					 columns=3,
					 blocks=[
						 #mobile_grid_block{ text=Name },
						 #mobile_grid_block{ text=io_lib:format("~.1fC",[T#temperature.value]) },
						 #mobile_grid_block{ text=io_lib:format("Updated ~.1f sec ago", [Diff])}
						]
					}
			     }],
	    
	    create_temperature(Tail, [ListElement|Acc])
    end.
			 
create_humidity([], Acc) ->
    Acc;

create_humidity([Head|Tail], Acc) ->

    {Id, Name} = Head,
    H = tellstick_server:get_humidity(Id),
    
    case H of
	not_found ->
	    create_humidity(Tail, Acc);
	_ ->
	    
	    Diff = timer:now_diff(erlang:now(), H#humidity.last_update_time)/1000000,
	    
	    ListElement = [#mobile_listitem{
			      theme=c,
			      body=   #mobile_grid { 
					 columns=3,
					 blocks=[
						 #mobile_grid_block{ text=Name },
						 #mobile_grid_block{ text=io_lib:format("~.1f%",[H#humidity.value]) },
						 #mobile_grid_block{ text=io_lib:format("Updated ~.1f sec ago", [Diff])}
						]
					}
			     }],
	    
	    create_humidity(Tail, [ListElement|Acc])
    end.
			 

			 

body() ->
         
    {ok, WantedDeviceList} = application:get_env(homeautomation, device),
    {ok, WantedTemperatureSensorList} = application:get_env(homeautomation, temperature),
    {ok, WantedHumiditySensorList} = application:get_env(homeautomation, humidity),

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
    wf:f(Elements++TempList++HumList).

event(Id) 
  when is_integer(Id)->
    Device = "device_" ++ integer_to_list(Id),
    IdAtom = list_to_atom(Device),
    SendCmd = wf:q(IdAtom),
    tellstick_server:device(Id, list_to_atom(SendCmd));

event({click, Button, Id}) ->
    ShowMenu = wf:q(Button),
    
    Menu = wf:q(list_to_atom("alarm_menu" ++ integer_to_list(Id))),
    io:format("ShowMenu ~p for button ~p menu ~p~n", [ShowMenu, Button, Menu]),
    case ShowMenu of
        "on" -> wf:wire(list_to_atom("alarm_menu" ++ integer_to_list(Id)),#slide_down{});
        _Other -> wf:wire(list_to_atom("alarm_menu" ++ integer_to_list(Id)),#slide_up{})
    end;

event(Other) ->    
    io:format("Other ~p~n", [Other]).
