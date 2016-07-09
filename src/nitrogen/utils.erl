%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(utils).
-include_lib("txrx/include/txrx.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib ("nitrogen_core/include/google_chart.hrl").
-compile(export_all).

xmlencode([], Acc) -> Acc; 
xmlencode([$<|T], Acc) -> xmlencode(T, Acc ++ "&lt;");
xmlencode([$å|T], Acc) -> xmlencode(T, Acc ++ "&aring;");
xmlencode([$ä|T], Acc) -> xmlencode(T, Acc ++ "&auml;");
xmlencode([$ö|T], Acc) -> xmlencode(T, Acc ++ "&ouml;");
xmlencode([$Å|T], Acc) -> xmlencode(T, Acc ++ "&Aring;");
xmlencode([$Ä|T], Acc) -> xmlencode(T, Acc ++ "&Auml;");
xmlencode([$Ö|T], Acc) -> xmlencode(T, Acc ++ "&Ouml;");
xmlencode([226,130,172|T], Acc) -> xmlencode(T, Acc ++ "&#8364;");
xmlencode([OneChar|T], Acc) -> xmlencode(T, lists:flatten([Acc,OneChar])). 


map_dir("0.0") -> "N";
map_dir("1.0") -> "NNE"; 
map_dir("2.0") -> "NE"; 
map_dir("3.0") -> "ENE"; 
map_dir("4.0") -> "E"; 
map_dir("5.0") -> "ESE"; 
map_dir("6.0") -> "SE";
map_dir("7.0") -> "SSE";
map_dir("8.0") -> "S"; 
map_dir("9.0") -> "SSW"; 
map_dir("10.0") -> "SW"; 
map_dir("11.0") -> "WSW"; 
map_dir("12.0") -> "W"; 
map_dir("13.0") -> "WNW"; 
map_dir("14.0") -> "NW"; 
map_dir("15.0") -> "NNW"; 
map_dir(_) -> "N".

dir_2_arrow("0.0") -> "&uarr;";
dir_2_arrow("1.0") -> "&nearr;";
dir_2_arrow("2.0") -> "&nearr;";
dir_2_arrow("3.0") -> "&nearr;";
dir_2_arrow("4.0") -> "&rarr;";
dir_2_arrow("5.0") -> "&searr;";
dir_2_arrow("6.0") -> "&searr;";
dir_2_arrow("7.0") -> "&searr;";
dir_2_arrow("8.0") -> "&darr;";
dir_2_arrow("9.0") -> "&swarr;";
dir_2_arrow("10.0") -> "&swarr;";
dir_2_arrow("11.0") -> "&swarr;";
dir_2_arrow("12.0") -> "&larr;";
dir_2_arrow("13.0") -> "&nwarr;";
dir_2_arrow("14.0") -> "&nwarr;";
dir_2_arrow("15.0") -> "&nwarr;";
dir_2_arrow(_) -> "&uarr;".
    

battery_string(Battery) when is_atom(Battery) ->
    atom_to_list(Battery);
battery_string(Battery) when is_integer(Battery) ->
    integer_to_list(Battery);
battery_string(Battery) when is_float(Battery) ->
    float_to_list(Battery);
battery_string(Battery) ->
    Battery.
    

get_sensor_values(Key, Type, Suffix) ->

    {ok, List} = application:get_env(homeautomation, Type),
    {Id, _Text} = proplists:get_value(Key, List),

    Rec = txrx_server:get_sensor(Id),
    case Rec of
	{sensor, Id, Val, _Prev, Min, Max, _Date, Battery, Upd} ->
	    
	    

	    Diff = timer:now_diff(erlang:now(), Upd)/1000000,
	    {val_to_string(Val), 
	     val_to_string(Min) ++ Suffix,
	     val_to_string(Max) ++ Suffix,
	     io_lib:format("~.1f sek", [Diff]),
	     Diff,
	     Battery
	    };

	_ -> 
	    {"--.-",
	     "--.-" ++ Suffix,
	     "--.-" ++ Suffix,
	     "--.-sek",
	     0.0,
	     unknown
	    }
    end.


get_rain(Key) ->
    {ok, List} = application:get_env(homeautomation, rain),
    {Id, _Text} = proplists:get_value(Key, List),

    Rec = txrx_server:get_sensor(Id),
    case Rec of
	{sensor, Id, _Val, _Prev, Min, Max, _Date, Battery, Upd} ->
	    Diff = timer:now_diff(erlang:now(), Upd)/1000000,
	    {val_to_string(Max - Min), 
	     io_lib:format("~.1f sek", [Diff]),
	     Diff,
	     Battery
	    };

	_ -> 
	    {"--.-",
	     "--.-sek",
	     0.0,
	     unknown
	    }
    end.

get_time_str() -> 
    {{_, _, _}, {H,M,_}} = calendar:local_time(),
    io_lib:format('~2..0b:~2..0b', [H, M]).


battery_warning(Battery) when is_atom(Battery) ->
    case Battery of
	good ->
	    {"#F5F5F5", 'info'};
	_ ->
	    {"#FAEC7F", 'alert'}
    end;
battery_warning(Battery) when is_integer(Battery) ->
    if
	Battery > 60 ->
	    {"#F5F5F5", 'info'};
	true ->
	    {"#FAEC7F", 'alert'}
    end;
	    
battery_warning(Battery) when is_float(Battery) ->
    if
	Battery > 60.0 ->
	    {"#F5F5F5", 'info'};
	true ->
	    {"#FAEC7F", 'alert'}
    end.


check_for_warnings(Diff, Battery) ->
    if
	Diff > 500.0 ->
	    {"#FAEC7F", 'alert'};
	true -> 
	    battery_warning(Battery)
    end.

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% TEMP OUT
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
temp_out(BigFontSize) ->
    {T1, _T2, _T3, _T4, Diff, Battery} = get_sensor_values(out, temperature, "&deg;C"), 
    {_H1, _H2, _H3, _H4, _H5, _Battery} = get_sensor_values(out, humidity, "%"), 

    {Color, Icon} = check_for_warnings(Diff, Battery),

    [#mobile_list{
	id=menu,
	theme=a,
	inset=true,
	body=[#mobile_list_divider {class=c,
				    body=[ 
					   %% #image { id=my_image_temp_out, 
					   %% 	    image="images/temperature.png"
					   %% 	  }, 
					   "  Temperatur Ute"]},
	      
	      #mobile_listitem{
		 style="background:" ++ Color ++ ";padding:0px !important;",
		 body=[
		       #span { 
			  body=[
				#table {
				   rows=[
					 #tablerow {
					    cells=[
						   #tablecell {body=#image { id=my_image_temp_out, 
									     image="images/temperature.png",
									     style="width:32px;height:32px;"
									   } 
							      }
						  ]},
					 #tablerow {
					    cells=[
						   #tablecell {
						      body=[
							    #button{id=button_temp_out_info,
								    postback=temp_out,
								    data_fields=[{icon,Icon},
										 {mini, true},
										 {inline, true},
										 {theme, c},
										 {iconpos, notext}]}
							   ]}
						  ]}
					]}
			       ],
			  style="display:inline-block;"
			 },
		       
		       #span { text=T1,
			       html_encode=false, 
			       style="font-size: " ++ integer_to_list(BigFontSize)
			       ++ "px; font-weight: bold;"},
		       #span { text="&deg;C ",
			       html_encode=false, 
			       style="font-size:35px; font-weight: bold;"}
		      ]}
	     ]
       }
    ].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% TEMP IN
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
temp_in(BigFontSize) ->
    {T1, _T2, _T3, _T4, Diff, Battery} = get_sensor_values(in, temperature, "&deg;C"), 
    {_H1, _H2, _H3, _H4, _H5, _Battery} = get_sensor_values(in, humidity, "%"), 

    {Color, Icon} = check_for_warnings(Diff, Battery),

    [#mobile_list{
	id=menu,
	theme=a,
	inset=true,
	body=[#mobile_list_divider {class=c, 
				    body=[ 
%% 					   #image { id=my_image_temp_in, 
%% 						    image="images/house.png"
%% 						    %%style="width:24px;height:24px;"
%% }, 
					   "  Temperatur Inne"]},
	      
	      #mobile_listitem{
		 style="background:" ++ Color ++ ";padding:0px !important;",
		 body=[
		       #span { 
			  body=[
				#table {
				   rows=[
					 #tablerow {
					    cells=[
						   #tablecell {body=#image { id=my_image_temp_in, 
									     image="images/house.png",
									     style="width:32px;height:32px;"
									   } 
							      }
						  ]},
					 #tablerow {
					    cells=[
						   #tablecell {
						      body=[
							    #button{id=button_temp_in_info,
								    postback=temp_in,
								    data_fields=[{icon,Icon},
										 {mini, true},
										 {inline, true},
										 {theme, c},
										 {iconpos, notext}]}
							   ]}
						  ]}
					]}
			       ],
			  style="display:inline-block;"
			 },		       
		       #span { text=T1,
			       html_encode=false, 
			       style="font-size: " ++ integer_to_list(BigFontSize)
			       ++ "px; font-weight: bold;"},
		       #span { text="&deg;C ",
			       html_encode=false, 
			       style="font-size: 35px; font-weight: bold;"}
		      ]}
	     ]
       }
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% RAIN
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rain(BigFontSize) ->
    {Rain, _Upd, Diff, Battery} = get_rain(out),

    {Color, Icon} = check_for_warnings(Diff, Battery),


    [#mobile_list{
	id=menu,
	theme=a,
	inset=true,
	body=[#mobile_list_divider {class=c, 
				    body=[ 
					   %% #image { id=my_image_rain, 
					   %% 	    image="images/rain.png"
					   %% 	  %%  style="width:24px;height:24px;"
					   %% 	  }, 
					   "  Regn Idag"]},
	      
	      #mobile_listitem{
		 style="background:" ++ Color ++ ";padding:0px !important;",
		 body=[
		       #span { 
			  body=[
				#table {
				   rows=[
					 #tablerow {
					    cells=[
						   #tablecell {body=#image { id=my_image_rain, 
									     image="images/rain.png",
									     style="width:32px;height:32px;"
									   } 
							      }
						  ]},
					 #tablerow {
					    cells=[
						   #tablecell {
						      body=[
							    #button{id=button_rain_info,
								    postback=rain,
								    data_fields=[{icon,Icon},
										 {mini, true},
										 {inline, true},
										 {theme, c},
										 {iconpos, notext}]}
							   ]}
						  ]}
					]}
			       ],
			  style="display:inline-block;"
			 },		       
		       #span { text=Rain,
			       html_encode=false, 
			       style="font-size: " ++ integer_to_list(BigFontSize)
			       ++ "px; font-weight: bold;"},
		       #span { text="mm",
			       html_encode=false, 
			       style="font-size: 35px; font-weight: bold;"}
		      ]}
	     ]
       }
    ].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% RAIN WITH CLOCK
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rain_with_clock(BigFontSize) ->
    {Rain, _Upd, Diff, Battery} = get_rain(out),

    {Color, Icon} = check_for_warnings(Diff, Battery),


    [#mobile_list{
	id=menu,
	theme=a,
	inset=true,
	body=[#mobile_list_divider {class=c, 
				    body=[ 
					   %% #image { id=my_image_rain, 
					   %% 	    image="images/rain.png"
					   %% 	  %%  style="width:24px;height:24px;"
					   %% 	  }, 
					   "  Regn Idag"]},
	      
	      #mobile_listitem{
		 style="background:" ++ Color ++ ";padding:0px !important;",
		 body=[
		       #span { 
			  body=[
				#table {
				   rows=[
					 #tablerow {
					    cells=[
						   #tablecell {body=#image { id=my_image_rain, 
									     image="images/rain.png",
									     style="width:32px;height:32px;"
									   } 
							      }
						  ]},
					 #tablerow {
					    cells=[
						   #tablecell {
						      body=[
							    #button{id=button_rain_info,
								    postback=rain,
								    data_fields=[{icon,Icon},
										 {mini, true},
										 {inline, true},
										 {theme, c},
										 {iconpos, notext}]}
							   ]}
						  ]}
					]}
			       ],
			  style="display:inline-block;"
			 },		       
		       #span { text=Rain,
			       html_encode=false, 
			       style="font-size: " ++ integer_to_list(BigFontSize)
			       ++ "px; font-weight: bold;"},
		       #span { text="mm",
			       html_encode=false, 
			       style="font-size: 35px; font-weight: bold;"}
		      ]},
	      #mobile_listitem{
		 style="padding:0px !important;",
		 body=[
		       #span {body=[
				    #image { id=my_image_clock, 
					     image="images/clock.png",
					     style="width:45px;height:45px;"}
				   ]},
		       #span { text=get_time_str(),
			       html_encode=false, 
			       style="font-size: " ++ integer_to_list(round(BigFontSize*0.45)) ++ 
				   "px; font-weight: bold;"}
		      ]}
	     ]
       }
    ].




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% WIND FULL
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wind(BigFontSize) ->
    {AvgVal, _AvgMin, _AvgMax, _AvgUpdated, Diff, Battery} = get_sensor_values(avg, wind, "m/s"), 
    {DirVal, _DirMin, _DirMax, _DirUpdated, _Diff, _Battery} = get_sensor_values(dir, wind, ""), 
        
    {Color, Icon} = check_for_warnings(Diff, Battery),

    [#mobile_list{
	id=menu,
	theme=a,
	inset=true,
	body=[#mobile_list_divider {class=c, 
				    body=[ 
					   %% #image { id=my_image_wind, 
					   %% 	    image="images/wind.png"},
					   
 					   #span{text="  Medelvind just nu"}
					 ]},
	      #mobile_listitem{
		 style="background:" ++ Color ++ ";padding:0px !important;",
		 body=[
		       #span { 
			  body=[
				#table {
				   rows=[
					 #tablerow {
					    cells=[
						   #tablecell {body=#image { id=my_image_wind, 
									     image="images/wind.png",
									     style="width:32px;height:32px;"
									   } 
							      }
						  ]},
					 #tablerow {
					    cells=[
						   #tablecell {
						      body=[
							    #button{id=button_wind_info,
								    postback=wind,
								    data_fields=[{icon,Icon},
										 {mini, true},
										 {inline, true},
										 {theme, c},
										 {iconpos, notext}]}
							   ]}
						  ]}
					]}
			       ],
			  style="display:inline-block;"
			 },		       
		       #span { text=AvgVal,
			       html_encode=false, 
			       style="font-size: " ++ integer_to_list(BigFontSize)
			       ++ "px; font-weight: bold;"},
		       #span { text="m/s",
			       html_encode=false, 
			       style="font-size: 35px; font-weight: bold;"}
		       
		       %% #span { text=" " ++ dir_2_arrow(DirVal),
		       %% 	     html_encode=false, 
		       %% 	     style="font-size: 25px; font-weight: bold;"}
		      ]},
	      #mobile_listitem{
		 style="background:" ++ Color ++ ";padding:0px !important;",
		 body=[
		       #span {body=[
				    #image { id=my_image_wind_direction_dir, 
					     image="images/dir_" ++ map_dir(DirVal) ++ ".png",
					     style="width:45px;height:45px;"}
				   ]},
		       #span { text="  " ++ map_dir(DirVal) ++ "  ",
			       html_encode=false, 
			       style="font-size: " ++ integer_to_list(round(BigFontSize*0.45)) ++ 
				   "px; font-weight: bold;"}
		      ]}
	      
	      
				     
	     ]
       }
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% CLOCK
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clock(BigFontSize) ->
    
    Color = "#F5F5F5",
    Icon = 'info',    
    
    [#mobile_list{
	id=menu,
	theme=a,
	inset=true,
	body=[#mobile_list_divider {class=c, 
				    body=[ 
					   "  Tid just nu"]},
	      
	      #mobile_listitem{
		 style="background:" ++ Color ++ ";padding:0px !important;",
		 body=[
		       #span { 
			  body=[
				#table {
				   rows=[
					 #tablerow {
					    cells=[
						   #tablecell {body=#image { id=my_image_clock, 
									     image="images/clock.png",
									     style="width:32px;height:32px;"
									   } 
							      }
						  ]},
					 #tablerow {
					    cells=[
						   #tablecell {
						      body=[
							    #button{id=button_clock_info,
								    postback=clock,
								    data_fields=[{icon,Icon},
										 {mini, true},
										 {inline, true},
										 {theme, c},
										 {iconpos, notext}]}
							   ]}
						  ]}
					]}
			       ],
			  style="display:inline-block;"
			 },		       
		       #span { text=get_time_str(),
			       html_encode=false, 
			       style="font-size: " ++ integer_to_list(BigFontSize) ++ 
				   "px; font-weight: bold;"}
		      ]}
	     ]
       }
    ].



info(temp_out) ->
    {T1, T2, T3, T4, _T5, Battery} = get_sensor_values(out, temperature, "&deg;C"), 
    {H1, H2, H3, _H4, _H5, _Battery} = get_sensor_values(out, humidity, "%"), 
    
    #panel { class=infoPanelTempOut, 
	     body=[
		   #h2 { text="Temperatur ute:" },
		   #label{text=T4 ++ " sedan uppdatering", html_encode=false},
		   #label{text=T1 ++ " ute temperatur just nu)", html_encode=false},
		   #label{text=T2 ++ " - " ++ T3 ++ " ute temperatur Min - Max idag)", html_encode=false},
		   #label{text=H1 ++ " ute luftfuktighet just nu", html_encode=false},
		   #label{text=H2 ++ " - " ++ H3 ++ " ute luftfuktighet Min - Max idag", html_encode=false},
		   #label{text="Batteri " ++ battery_string(Battery) ++ "%", html_encode=false},
		   #label{text=xmlencode("(Ta bort detta: Tryck reload knappen eller vänta en minut)", []), html_encode=false}
		  ]};
        
info(temp_in) ->
    {T1, T2, T3, T4, _T5, Battery} = get_sensor_values(in, temperature, "&deg;C"), 
    {H1, H2, H3, _H4, _H5, _Battery} = get_sensor_values(in, humidity, "%"), 
    
    #panel { class=infoPanelTempIn, 
	     body=[
		   #h2 { text="Temperatur Inne:" },
		   #label{text=T4 ++ " sedan uppdatering", html_encode=false},
		   #label{text=T1 ++ " inne temperatur just nu)", html_encode=false},
		   #label{text=T2 ++ " - " ++ T3 ++ " inne temperatur Min - Max idag)", html_encode=false},
		   #label{text=H1 ++ " inne luftfuktighet just nu", html_encode=false},
		   #label{text=H2 ++ " - " ++ H3 ++ " inne luftfuktighet Min - Max idag", html_encode=false},
		   #label{text="Batteri " ++ battery_string(Battery) ++ "%", html_encode=false},
		   #label{text=xmlencode("(Ta bort detta: Tryck reload knappen eller vänta en minut)", []), html_encode=false}
		  ]};
        

info(rain) ->
    {Rain, Upd, _Diff, Battery} = get_rain(out),

    #panel { class=infoPanelRain, 
	     body=[
		   #h2 { text="Regn:" },
		   #label{text=Upd ++ " sedan uppdatering", html_encode=false},
		   #label{text=Rain ++ " Regn idag", html_encode=false},
		   #label{text="Batteri " ++ battery_string(Battery) ++ "%", html_encode=false},
		   #label{text=xmlencode("(Ta bort detta: Tryck reload knappen eller vänta en minut)", []), html_encode=false}
		  ]};

info(wind) ->
    {GustVal, GustMin, GustMax, _GustUpdated, _Diff1, Battery} = get_sensor_values(gust, wind, "m/s"), 
    {AvgVal, AvgMin, AvgMax, _AvgUpdated, _Diff2, _Battery1} = get_sensor_values(avg, wind, "m/s"), 
    {DirVal, _DirMin, _DirMax, DirUpdated, _Diff3, _Battery2} = get_sensor_values(dir, wind, ""), 
    
    #panel { class=infoPanelWind, 
	     body=[
		   #h2 { text="Vind:" },
		   #label{text=DirUpdated ++ " sedan uppdatering", html_encode=false},
		   #label{text=GustVal ++ " Vindby just nu", html_encode=false},
		   #label{text=GustMin ++ " - " ++ GustMax ++ " vindby Min - Max idag)", html_encode=false},
		   #label{text=AvgVal ++ " Medelvind just nu", html_encode=false},
		   #label{text=AvgMin ++ " - " ++ AvgMax ++ " medelvind Min - Max idag", html_encode=false},
		   #label{text=map_dir(DirVal) ++ " Vindriktning just nu", html_encode=false},
		   #label{text="Batteri " ++ battery_string(Battery) ++ "%", html_encode=false},
		   #label{text=xmlencode("(Ta bort detta: Tryck reload knappen eller vänta en minut)", []), html_encode=false}
		  ]};

info(Other) ->
    #panel { class=infoPanelOther, 
	     body=[
		   #h2 { text="No Info!" }
		  ]}.

    

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

    {ok, List1} = application:get_env(homeautomation, temperature),
    {TempId, _Text1} = proplists:get_value(out, List1),
    Temp = mysql_server:get_min_max_last_week(TempId),

    {ok, List2} = application:get_env(homeautomation, rain),
    {RainId, _Text2} = proplists:get_value(out, List2),
    Rain = mysql_server:get_min_max_last_week(RainId),
    Ret = acc_text(Temp, Rain, []),
    Ret.
