-module(primes_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../src/states.hrl").

-define(CONF, "../config/primes.config").
%% ===================================================================
%%				TESTS
%% ===================================================================
all_test_() ->
	[
		test_config(), % проверка наличия конфигурации
		test_config_keys(), % наличие всех параметров
		test_config_content(), % типы параметров
		test_app_start()% старт приложения (старт, старт eredis, gs-ов и т.п.)
		% генерация (наличие, диапазон, помещение в очередь, событие Redis)
		% частота генерации случайных чисел
	].

%---------------------------------------------------------------------
% Тест файла конфигурации (есть ли и читается ли)
%---------------------------------------------------------------------	
test_config() ->
	[
		% есть конфиг?
		?_assertEqual(true, filelib:is_file(?CONF)),
		% открывается как список термов?
		?_assertMatch({ok,_}, file:consult(?CONF))
	]. 
%---------------------------------------------------------------------
% Тест содержимого файла конфигурации
%---------------------------------------------------------------------
test_config_keys() ->
	{ok, [Args]} = file:consult(?CONF),
	KeysInConfig = proplists:get_keys(Args),
	KeysShouldBe =  ['RedisDB','RedisHost','RedisPort','QueueKey', 'ResultSetKey','N'],
	[?_assertEqual(KeysInConfig, KeysShouldBe)].

test_config_content() ->
    {ok, [Args]} = file:consult(?CONF),
    RedisHost    = proplists:get_value('RedisHost'   , Args, "127.0.0.1"),
    RedisPort    = proplists:get_value('RedisPort'   , Args, 6379       ),
    RedisDB      = proplists:get_value('RedisDB'     , Args, 0          ),% 0..15
    QueueKey     = proplists:get_value('QueueKey'    , Args, "randoms"  ),
    ResultSetKey = proplists:get_value('ResultSetKey', Args, "primes"   ),
	N			 = proplists:get_value('N'           , Args, 1000000000 ),
    [
        ?_assertMatch({ok, _}, inet:parse_ipv4_address(RedisHost)),
        ?_assert(is_integer(RedisPort) andalso RedisPort >= 0 andalso RedisPort =< 65535),
        ?_assert(is_integer(RedisDB) andalso RedisDB >=0 andalso RedisDB =< 15),
        ?_assert(is_list(QueueKey)),
        ?_assert(is_list(ResultSetKey)),
        ?_assert(is_integer(N) andalso N >= 2)
    ].

test_app_start() ->
    {ok, [Args]} = file:consult(?CONF),
    RedisHost    = proplists:get_value('RedisHost'   , Args, "127.0.0.1"),
    RedisPort    = proplists:get_value('RedisPort'   , Args, 6379       ),
    RedisDB      = proplists:get_value('RedisDB'     , Args, 0          ),% 0..15
    QueueKey     = proplists:get_value('QueueKey'    , Args, "randoms"  ),
    ResultSetKey = proplists:get_value('ResultSetKey', Args, "primes"   ),
	N			 = proplists:get_value('N'           , Args, 1000000000 ),
    [
        ?_assertMatch({ok, _}, eredis:start_link(RedisHost, RedisPort, RedisDB)),
        ?_assertMatch({ok, _}, eredis_sub:start_link()),
        ?_assertMatch({ok, _}, eredis_smart_sub:start_link(EredisSubClient)),
        ?_assertEqual(ok, application:start(primes))
    ].

prepare() ->
    {ok, [Args]} = file:consult(?CONF),
    RedisHost    = proplists:get_value('RedisHost'   , Args, "127.0.0.1"),
    RedisPort    = proplists:get_value('RedisPort'   , Args, 6379       ),
    RedisDB      = proplists:get_value('RedisDB'     , Args, 0          ),% 0..15
    QueueKey     = proplists:get_value('QueueKey'    , Args, "randoms"  ),
    ResultSetKey = proplists:get_value('ResultSetKey', Args, "primes"   ),
	N			 = proplists:get_value('N'           , Args, 1000000000 ),
    {ok, Eredis} = eredis:start_link(RedisHost, RedisPort, RedisDB),
    % получить PID процесса-контроллера подписки на события eredis
    {ok, EredisSubClient} = eredis_sub:start_link(),
    {ok, SubClient} = eredis_smart_sub:start_link(EredisSubClient),
    RG = #st_rg{
        number = N,
        queue  = list_to_binary(QueueKey),
        redis  = Eredis
    },
    PF = #st_pf{     
        queue     = list_to_binary(QueueKey),     % для подписки на события
        set       = list_to_binary(ResultSetKey),
        redis     = Eredis,
        redis_sub = SubClient
    },
    {Args, RG, PF}.
finish(_) ->
    no_op.

%test_config_contents_start() -> 
%	['RedisDB','RedisHost','RedisPort','QueueKey', 'ResultSetKey','N'].
%test_config_contents_stop(_) -> no_op.
%
%test_config_contents() ->
%	{"Тест содержимого конфига",
%		{setup,
%		fun prepare/0,
%		fun finish/1,
%		fun (ParamList) ->
%			[
%				{
%					"% все ключи на месте?",
%					?_assertEqual(ParamList,  proplists:get_keys(file:consult(?CONF)))
%				}
%			]
%		end}
%	}.