-module(administrador).
-export([ejecutar/3,evaluar/2]).

%%-----------------------------------------------------------------
evaluar(Nodo,[]) ->
	io:format("~p> Debe ingresar algun hecho.~n",[Nodo]);

evaluar(Nodo,Hechos) ->
	T1 = get_timestamp(),
%	io:format("~p>Inicio invocacion: ~p~n",[Nodo,T1]),	
	{evaluador, Nodo} ! {admin, self(),Hechos},
	receive	
		{nodo,HechosResultantes} ->  
					T2 = get_timestamp(),
					DIFF = T2 - T1,
%					io:format("~p>Fin invocacion: ~p~n",[Nodo,T2]),
					io:format("~p>Hechos resultantes: ~p~n",[Nodo,HechosResultantes]),
					io:format("~p>Tiempo de resolucion: ~p us~n",[Nodo,DIFF])
	end.
%%-----------------------------------------------------------------
get_timestamp() ->
    		{Mega,Sec,Micro} = erlang:now(),
    		(Mega*1000000+Sec)*1000000+Micro.
%%-----------------------------------------------------------------
ejecutar(Nodo1,Nodo2,Hechos) -> 
	io:format("Hechos iniciales: ~p~n",[Hechos]),
	spawn(administrador,evaluar,[Nodo1, Hechos]),         
	spawn(administrador,evaluar,[Nodo2, Hechos]).
