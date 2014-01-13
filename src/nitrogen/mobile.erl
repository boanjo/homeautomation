%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(mobile).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).
-author("Anders Johansson (gumm@sigma-star.com)").

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

    {device, Id, Name, State, _Value, _LastStateChange} = Head, 
    ListElement = [#label { text=Name, html_encode=true },
		   #mobile_toggle{
		      on_text="on",
		      off_text="off",
		      selected=map_state(State),
		      postback=Id,
		      id=list_to_atom("device_" ++ integer_to_list(Id)),
		      width=100
		     },
		   #hr{}],
    create_device(Tail, [ListElement|Acc]).
			 

body() ->
    
     
    Elements = create_device(tellstick_server:get_all_devices(), []),
 
    Now = erlang:now(),
    {_, TempIn, LastInNow} = tellstick_server:get_temperature(in),
    {_, TempOut, LastOutNow} = tellstick_server:get_temperature(out),
    {_, HumIn, _} = tellstick_server:get_humidity(in),
    {_, HumOut, _} = tellstick_server:get_humidity(out),

    InDiff = timer:now_diff(Now, LastInNow)/1000000,
    OutDiff = timer:now_diff(Now, LastOutNow)/1000000,
    
    

    TempList = [#mobile_list{
		   id=menu,
		   theme=a,
		   inset=true,
		   %%    		style="display:none",
		   body=[
			 #mobile_list_divider{class=c, text="Temperatur"},
			 #mobile_listitem{
			    theme=c,
			    body=   #mobile_grid { 
				       columns=3,
				       blocks=[
					       #mobile_grid_block{ text="Ute" },
					       #mobile_grid_block{ text=io_lib:format("~.1fC",[TempOut]) },
					       #mobile_grid_block{ text=io_lib:format("Updated ~.1f sec ago", [OutDiff])}
					      ]
				      }
			   },
			 #mobile_listitem{
			    theme=c,
			    body=   #mobile_grid { 
				       columns=3,
				       blocks=[
					       #mobile_grid_block{ text="Inne" },
					       #mobile_grid_block{ text=io_lib:format("~.1fC",[TempIn]) },
					       #mobile_grid_block{ text=io_lib:format("Updated ~.1f sec ago", [InDiff])}
					      ]
				      }
			   }
			 
			]
		  }
	       ],
    HumList = [#mobile_list{
		  id=menu,
		  theme=a,
		  inset=true,
		  %%    		style="display:none",
		  body=[
			#mobile_list_divider{class=c, text="Luftfuktighet"},
			#mobile_listitem{
			   theme=c,
			   body=   #mobile_grid { 
				      columns=3,
				      blocks=[
					      #mobile_grid_block{ text="Ute" },
					      #mobile_grid_block{ text=io_lib:format("~.1f%",[HumOut]) },
					      #mobile_grid_block{ text=io_lib:format("Updated ~.1f sec ago", [OutDiff])}
					     ]
				     }
			  },
			#mobile_listitem{
			   theme=c,
			   body=   #mobile_grid { 
				      columns=3,
				      blocks=[
					      #mobile_grid_block{ text="Inne" },
					      #mobile_grid_block{ text=io_lib:format("~.1f%",[HumIn]) },
					      #mobile_grid_block{ text=io_lib:format("Updated ~.1f sec ago", [InDiff])}
					     ]
				     }
			  }
			
		       ]
		 }
	      ],
    wf:f(Elements++TempList++HumList).

event(Device) 
  when is_integer(Device)->
    Id = "device_" ++ integer_to_list(Device),
    IdAtom = list_to_atom(Id),
    SendCmd = wf:q(IdAtom),
    case SendCmd of
        "on" -> tellstick_server:send_to_port([1,Device]);
        "off" -> tellstick_server:send_to_port([2,Device])
    end;

event({click, goButton}) ->
    ShowMenu = wf:q(checkbox1),
    case ShowMenu of
        "on" -> wf:wire(menu,#appear{});
        "off" -> wf:wire(menu,#fade{})
    end;
    
event(Other) ->
    
    io:format("Other ~p~n", [Other]).
