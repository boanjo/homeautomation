%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(weather).
-include_lib("txrx/include/txrx.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib ("nitrogen_core/include/google_chart.hrl").
-compile(export_all).
-author("Anders Johansson (epkboan@gmail.com)").

main() -> #template{file="./priv/templates/weather.html"}.

title() -> "Oxelgatan 7".

footer() -> "&copy; 2014 - Anders Johansson".
    


get_sensor_values(Key, Type, Suffix) ->
    {ok, List} = application:get_env(homeautomation, Type),
    {Id, _Text} = proplists:get_value(Key, List),
    
    Rec = txrx_server:get_sensor(Id),
    case Rec of
	{sensor, Id, Val, _Prev, Min, Max, _Date, Upd} ->
	    Diff = timer:now_diff(erlang:now(), Upd)/1000000,
	    {val_to_string(Val) ++ Suffix, 
	     val_to_string(Min) ++ Suffix,
	     val_to_string(Max) ++ Suffix,
	     io_lib:format("~.1f sek", [Diff])};
	
	_ -> 
	    {"--.-" ++ Suffix,
	     "--.-" ++ Suffix,
	     "--.-" ++ Suffix,
	     "--.-sek"
	    }
    end.


get_rain(Key) ->
    {ok, List} = application:get_env(homeautomation, rain),
    {Id, _Text} = proplists:get_value(Key, List),
    
    Rec = txrx_server:get_sensor(Id),
    case Rec of
	{sensor, Id, _Val, _Prev, Min, Max, _Date, Upd} ->
	    Diff = timer:now_diff(erlang:now(), Upd)/1000000,
	    {val_to_string(Max - Min) ++ "mm", 
	     io_lib:format("~.1f sek", [Diff])
	    };
	
	_ -> 
	    {"--.-mm",
	     "--.-sek"
	    }
    end.



create_mobile_list(Large, Rows) ->
    [#mobile_listitem{
       theme=c,
       body=   #mobile_grid { 
		  columns=2,
		  blocks=[
			  #mobile_grid_block{ body=[#label { text=Large,
							     html_encode=false, 
							     style="font-size: 75px; font-weight: bold;"}]},
			  #mobile_grid_block{ body=Rows }
			 ]
		 }
      }].
    
    


body() ->
    
    {To1, To2, To3, To4} = get_sensor_values(out, temperature, "&deg;C"), 
    {Ti1, Ti2, Ti3, Ti4} = get_sensor_values(in, temperature, "&deg;C"), 
    {Ho1, Ho2, Ho3, _Ho4} = get_sensor_values(out, humidity, "%"), 
    {Hi1, Hi2, Hi3, _Hi4} = get_sensor_values(in, humidity, "%"), 
    {Rain, Upd} = get_rain(out),
    

    Rows1 = [#table { 
		rows=[ 
		       #tablerow { 
			  cells=[
				 #tablecell { body=#label { text=To4 ++ " sedan uppdatering", html_encode=false}}
				]},
		       #tablerow { 
			  cells=[
				 #tablecell { body=#label { text=Ho1 ++ " luftfuktighet"}}
				]},
		       #tablerow { 
			  cells=[
				 #tablecell { body=#label { text=To2 ++ " - " ++ To3 ++ " (Min/Max)", html_encode=false}}
				]},
		       #tablerow { 
			  cells=[
				 #tablecell { body=#label { text=Ho2 ++ " - " ++ Ho3 ++ " (Min/Max)", html_encode=false}}
				 
				 
				]}
		     ]}],
    
    
    
    TempList = [#mobile_list{
		   id=menu,
		   theme=a,
		   inset=true,
		   %%    		style="display:none",
		   body=[#mobile_list_divider{class=c, text="Ute"}] ++ 
		       create_mobile_list(To1, Rows1)

		   
		   
		  }
	       ],

    Rows2 = [#table { 
		rows=[ 
		       #tablerow { 
			  cells=[
				 #tablecell { body=#label { text=Ti4 ++ " sedan uppdatering", html_encode=false}}
				]},
		       #tablerow { 
			  cells=[
				 #tablecell { body=#label { text=Hi1 ++ " luftfuktighet", html_encode=false}}
				]},
		       #tablerow { 
			  cells=[
				 #tablecell { body=#label { text=Ti2 ++ " - " ++ Ti3 ++ " (Min/Max)", html_encode=false}}
				]},
		       
		       #tablerow { 
			  cells=[
				 #tablecell { body=#label { text=Hi2 ++ " - " ++ Hi3 ++ " (Min/Max)", html_encode=false}}
				 
				 
				]}
		     ]}],
    
    TempList2 = [#mobile_list{
		    id=menu,
		   theme=a,
		   inset=true,
		   %%    		style="display:none",
		   body=[#mobile_list_divider{class=c, text="Inne"}] ++ 
		       create_mobile_list(Ti1, Rows2)

		   
		   
		  }
	       ],


    Rows3 = [#table { 
		rows=[ 
		       #tablerow { 
			     cells=[
				    #tablecell { body=#label { text=Upd ++ " sedan uppdatering", html_encode=false}}
				   ]}
		       
		     ]
	       }],


    TempList3 = [#mobile_list{
		   id=menu,
		   theme=a,
		   inset=true,
		   %%    		style="display:none",
		   body=[#mobile_list_divider{class=c, text="Regn idag"}] ++ 
		       create_mobile_list(Rain, Rows3)

		   
		   
		  }
	       ],
    wf:f(TempList ++ TempList2 ++ TempList3).



val_to_string(Val) when is_integer(Val) ->
    integer_to_list(Val) ++ ".0";
    
val_to_string(Val) when is_float(Val) ->
    float_to_list(Val, [{decimals, 1}, compact]);

val_to_string(Val) when is_atom(Val) ->
    "--.-".   

acc_text([], [], Acc) ->
    Acc;
acc_text(_Temp, [], Acc) ->
    Acc;
acc_text([], _Rain, Acc) ->
    Acc;
acc_text([TempHead|TempTail], [RainHead|RainTail], Acc) ->
    
    Tail = case Acc of 
	       [] ->
		   [];
	       _ -> ","
	   end,
			
    [{date, {_Y, M, D}}, TMax, TMin] = TempHead,

    [{date, {_Y, _M, _D}}, RMax, RMin] = RainHead,

   %% Row = io_lib:format("['~p/~p', ~p, ~p, ~p, ~p]", [D,M,TMax,TMin,TMax,(RMax-RMin)]),

    Row = "['" ++ integer_to_list(D) ++ "/" 
	++ integer_to_list(M) ++ "', " 
    	++ val_to_string(RMax-RMin) ++ ", "
	++ val_to_string(TMax) ++ ", " 
    	++ val_to_string(TMin) ++ ", " 
	++ val_to_string(TMin) ++ ", " 
	++ val_to_string(TMax) ++ "]",
  
    NewAcc = Acc ++ Tail ++ Row,
    acc_text(TempTail, RainTail, NewAcc).

get_last_week() ->
    
    Temp = mysql_server:get_min_max_last_week(133),
    Rain = mysql_server:get_min_max_last_week(152),
    Ret = acc_text(Temp, Rain, []),
    Ret.


event(my_click_event) ->  
    error_logger:info_msg("Click ~p~n", [my]);

event(Other) ->    
    error_logger:info_msg("Other ~p~n", [Other]).
