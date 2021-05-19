%%%-------------------------------------------------------------------
%% @doc primes public API
%% @end
%%%-------------------------------------------------------------------

-module(primes_app).

-behaviour(application).

-include("include/states.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	{ok, Conf}   = application:get_env(primes, configuration),
	{ok, [Args]} = file:consult(Conf),
	
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

	primes_sup:start_link([
							{primes_filter_args   , PF}, 
						 	{random_generator_args, RG}
						]).			
	
stop(_State) ->
    ok.

%% internal functions
