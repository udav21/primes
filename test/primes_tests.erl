-module(primes_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../include/states.hrl").

-define(FCONF, "primes.config").

%% ===================================================================
%%				TESTS
%% ===================================================================
all_test_() ->
	[
		test_config(),          % проверка наличия конфигурации
		test_config_keys(),     % наличие всех параметров
        test_config_content(),  % типы параметров
		test_rg(),              % генерация (наличие, диапазон, помещение в очередь, событие Redis)
		test_app_start()        % старт приложения (старт, стоп)
	].

%---------------------------------------------------------------------
% Тест файла конфигурации (есть ли и читается ли)
%---------------------------------------------------------------------	
test_config() ->    
	[
		% есть конфиг?
		?_assertEqual(true, filelib:is_file(?FCONF)),
		% открывается как список термов?
		?_assertMatch({ok,_}, file:consult(?FCONF))
	]. 
%---------------------------------------------------------------------
% Тест содержимого файла конфигурации
%---------------------------------------------------------------------
test_config_keys() ->
	{ok, [Args]} = file:consult(?FCONF),
	KeysInConfig = proplists:get_keys(Args),
	KeysShouldBe =  ['RedisDB','RedisHost','RedisPort','QueueKey', 'ResultSetKey','N'],
	[?_assertEqual(KeysInConfig, KeysShouldBe)].

test_config_content() ->
    Pars = get_params(),
    [
        ?_assertMatch({ok, _}, inet:parse_ipv4_address(Pars#args.rds_host)),
        ?_assert(is_integer(Pars#args.rds_port) andalso 
                            Pars#args.rds_port >= 0 andalso 
                            Pars#args.rds_port =< 65535),
        ?_assert(is_integer(Pars#args.rds_db) andalso 
                            Pars#args.rds_db >=0 andalso
                            Pars#args.rds_db =< 15),
        ?_assert(is_list(Pars#args.rds_q)),
        ?_assert(is_list(Pars#args.rds_s)),
        ?_assert(is_integer(Pars#args.num) andalso 
                            Pars#args.num >= 2)
    ].

test_rg() ->
    BasicArgs = add_redis_pids(get_params()),
    ArgsRG = prepare_record_rg(BasicArgs),
    %ArgsPF = prepare_record_pf(BasicArgs),
    {ok, _RG} = random_generator:start_link(ArgsRG).

test_app_start() ->
    [
        ?_assertEqual(ok, application:start(primes)),
        ?_assertEqual(ok, application:stop(primes))
    ].

%test_rg() ->
%	{"Тест содержимого конфига",
%		{setup,
%		fun test_rg_start/0,
%		fun test_rg_stop/1,
%		fun (ParamList) ->
%			[
%				{
%					"% все ключи на месте?",
%					?_assertEqual(ParamList,  proplists:get_keys(file:consult(?CONF)))
%				}
%			]
%		end}
%    }.
%%-------------------------------------------------------------------------
%% Функции подготовки структур данных
%%-------------------------------------------------------------------------

get_params() -> 
    {ok, [Args]} = file:consult(?FCONF),
    Pars = #args{
                    rds_host = proplists:get_value('RedisHost'   , Args, "127.0.0.1"),
                    rds_port = proplists:get_value('RedisPort'   , Args, 6379       ),
                    rds_db   = proplists:get_value('RedisDB'     , Args, 0          ),
                    rds_q    = proplists:get_value('QueueKey'    , Args, "randoms"  ),
                    rds_s    = proplists:get_value('ResultSetKey', Args, "primes"   ),
	                num		 = proplists:get_value('N'           , Args, 1000000000 )
               }.

add_redis_pids(#args{} = Pars) ->
    application:load(eredis),
    {ok, Eredis} = eredis:start_link(Pars#args.rds_host,
                                     Pars#args.rds_port, 
                                     Pars#args.rds_db),    
    {ok, EredisSubClient} = eredis_sub:start_link(),
    {ok, SubClient} = eredis_smart_sub:start_link(EredisSubClient),
    NewPars = Pars#args{rds = Eredis, rds_sub = SubClient}.

prepare_record_rg(#args{} = Pars) ->
    RG = #st_rg{
                number = Pars#args.num,
                queue  = list_to_binary(Pars#args.rds_q),
                redis  = Pars#args.rds
               }.
prepare_record_pf(#args{} = Pars) ->
    PF = #st_pf{     
                queue     = list_to_binary(Pars#args.rds_q), 
                set       = list_to_binary(Pars#args.rds_s),
                redis     = Pars#args.rds,
                redis_sub = Pars#args.rds_sub
               }.