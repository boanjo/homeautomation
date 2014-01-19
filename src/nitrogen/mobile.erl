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

create_device([], Acc) ->
    Acc;

create_device([Head|Tail], Acc) ->

    {Id, Name, _AlarmList} = Head,   
    Device = tellstick_server:get_device(Id),
    
    ListElement = [#label { text=Name, html_encode=true },
		   #mobile_toggle{
		      on_text="on",
		      off_text="off",
		      selected=map_state(Device#device.state),
		      postback=Id,
		      id=list_to_atom("device_" ++ integer_to_list(Id)),
		      width=100
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

event({click, goButton}) ->
    ShowMenu = wf:q(checkbox1),
    case ShowMenu of
        "on" -> wf:wire(menu,#appear{});
        "off" -> wf:wire(menu,#fade{})
    end;
    
event(Other) ->
    
    io:format("Other ~p~n", [Other]).
