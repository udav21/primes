-module(random_generator).

-behaviour(gen_server).

-include("states.hrl").

-define(SERVER, ?MODULE).

-define(INTERVAL, 333). % интервал в микросекундах между "тиками" 

-export([start_link/1]).

-export([init/1, 
         handle_call/3,
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).


start_link(Args) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

init(State) ->
    % запуск генератора "тиков", PID для kill в terminate()
    Ticker =  spawn( fun() -> ticker(?SERVER, erlang:monotonic_time()) end),
    NewState = State#st_rg{ticker = Ticker},
    {ok, NewState}.

handle_call(_Request, _From, State) ->	
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    % получение случайного числа
    Number  = 1 + rand:uniform(State#st_rg.number-1),
    % помещение числа в очередь
    eredis:q(State#st_rg.redis, ["LPUSH", State#st_rg.queue, Number]),
    % публикация события в канал сообщений очереди
    % ключ <<"take_number">> для идентификации сообщений
    % можно оставить захардкоженным как здесь или поместить в State
    eredis:q(State#st_rg.redis, ["PUBLISH", State#st_rg.queue, <<"take_number">>]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%=========================================================================
%% Внутренние функции
%%=========================================================================
%--------------------------------------------------------------------------
% @doc
% Функция в бесконечном цикле генерирует события с заданным интервалом и
% с коррекцией времени задержки, и посылает сообщение процессу-обработчику.
% @end
% -------------------------------------------------------------------------
-spec ticker(Sender, Time1) -> no_return() when
    Sender :: pid(), Time1 :: integer().
ticker(Sender, Time1) ->
        Time2 = erlang:monotonic_time(),
	    Int = erlang:convert_time_unit(Time2 -Time1, native, micro_seconds),
	    if
	    	Int < ?INTERVAL ->
	    		ticker(Sender, Time1);
	    	true ->
                Sender ! tick,
                ticker(Sender, Time2)
	    end.
