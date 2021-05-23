-module(primes_tests).

-include("include/states.hrl").

-define(FCONF, "primes.config").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


%% ========================================================================
%%				TESTS
%% ========================================================================
all_test_() ->
	[
        test_config(),           % проверка наличия конфигурации
        test_config_content(),   % типы параметров
        test_rg(),               % темп генерации случайных чисел
        test_pf()                % работа алгоритма фильтрации простых чисел
	].

%--------------------------------------------------------------------------
% Тест файла конфигурации (есть ли и читается ли)
%--------------------------------------------------------------------------	
test_config() ->    
	[
		{"Is there a config file?",
		    ?_assertEqual(true, filelib:is_file(?FCONF))
        },
		{"Can the terms list be opened?",
		    ?_assertMatch({ok,_}, file:consult(?FCONF))
        }
	]. 

test_config_content() ->
    {"Testing contents of the parameters",
        {setup,
            fun get_params/0,
            fun (Pars) ->
                [
                    {"% IPv4 ?",
                     ?_assertMatch({ok, _}, inet:parse_ipv4_address(Pars#args.rds_host))},
                    {"% Port ?",
                     ?_assert(is_integer(Pars#args.rds_port) andalso 
                                         Pars#args.rds_port >= 0 andalso 
                                         Pars#args.rds_port =< 65535)},
                    {"% Database number ?",
                     ?_assert(is_integer(Pars#args.rds_db) andalso 
                                         Pars#args.rds_db >=0 andalso
                                         Pars#args.rds_db =< 15)},
                    {"% Queue name ?",
                     ?_assert(is_list(Pars#args.rds_q))},
                    {"% Set name ?",
                     ?_assert(is_list(Pars#args.rds_s))},
                    {"% Number ?",
                     ?_assert(is_integer(Pars#args.num) andalso 
                                         Pars#args.num >= 2)}
                ]
            end
        }
    }.

test_rg() ->
    BasicArgs = add_redis_pids(get_params()),
    StateRG = prepare_record_rg(BasicArgs),
    StatePF = prepare_record_pf(BasicArgs),
    % подписаться на сообщения очереди "в параллель" с запущенным генератором
    gen_server:cast(StatePF#st_pf.redis_sub, {subscribe, [StatePF#st_pf.queue], self()}),
    random_generator:start_link(StateRG),
    Time1 = erlang:monotonic_time(),
    count(3000), % отсчитать 3000 "тиков"
    Time2 = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(Time2 -Time1, native, micro_seconds),
    % отписаться от сообщений очереди
    gen_server:cast(StatePF#st_pf.redis_sub, {unsubscribe, [StatePF#st_pf.queue], self()}),
    % отсчитанные 3000 "тиков" поделить на затраченное время
    Rate = 3000*1000000/Time, % на интервале в 1 секунду
    [ 
        {"Generation rate 3000 tps",
         ?_assert((Rate > 2910) and (Rate < 3090))} % погрешность +- 3%
         % относительная погрешность падает с ростом длительности интервала генерации
         % дело в том, что генератор даёт абсолютную погрешность приблизительно в 
         % 10 - 50 "тиков" в секунду в зависимости от "железа" и операционной системы.
    ].


test_pf() ->
    [
        {"Strong pseudoprime 9746347772161",
         ?_assertNot(primes_filter:is_prime(9746347772161))},       % одно из Кармайкловых чисел
        {"Strong pseudoprime 443372888629441",
         ?_assertNot(primes_filter:is_prime(443372888629441))},     % сильное псевдопростое Фибоначчи
        {"Strong pseudoprime 3825123056546413051",
         ?_assertNot(primes_filter:is_prime(3825123056546413051))}, % сильное псевдопростое по 9-ти основания (первым 9-ти простым)
        {"Prime 18014398241046527",
         ?_assert(primes_filter:is_prime(18014398241046527))},       % простое
        {"Prime 4398042316799",
         ?_assert(primes_filter:is_prime(4398042316799))}            % простое
    ].


%%=========================================================================
%%-------------------------------------------------------------------------
%% Функции подготовки структур данных
%%-------------------------------------------------------------------------

get_params() -> 
    {ok, [Args]} = file:consult(?FCONF),
    #args{
            rds_host = proplists:get_value('RedisHost'   , Args, "127.0.0.1"),
            rds_port = proplists:get_value('RedisPort'   , Args, 6379       ),
            rds_db   = proplists:get_value('RedisDB'     , Args, 0          ),
            rds_q    = proplists:get_value('QueueKey'    , Args, "randoms"  ),
            rds_s    = proplists:get_value('ResultSetKey', Args, "primes"   ),
	        num		 = proplists:get_value('N'           , Args, 1000000000 )
         }.

add_redis_pids(#args{} = Pars) ->
    application:load(eredis),
    application:load(eredis_sub),
    application:load(eredis_smart_sub),
    {ok, Eredis} = eredis:start_link(Pars#args.rds_host,
                                     Pars#args.rds_port, 
                                     Pars#args.rds_db),    
    {ok, EredisSubClient} = eredis_sub:start_link(),
    {ok, SubClient} = eredis_smart_sub:start_link(EredisSubClient),
    Pars#args{rds = Eredis, rds_sub = SubClient}.

prepare_record_rg(#args{} = Pars) ->
    #st_rg{
            number = Pars#args.num,
            queue  = list_to_binary(Pars#args.rds_q),
            redis  = Pars#args.rds
          }.
prepare_record_pf(#args{} = Pars) ->
    #st_pf{     
            queue     = list_to_binary(Pars#args.rds_q), 
            set       = list_to_binary(Pars#args.rds_s),
            redis     = Pars#args.rds,
            redis_sub = Pars#args.rds_sub
          }.

count(0) -> ok;
count(Ticks) ->
       receive
           {message, <<"take_number">>} -> count(Ticks - 1)
       after 5000 ->
           timeout
       end.
-endif.