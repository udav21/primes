-module(primes_filter).

-define(SERVER, ?MODULE).

-behaviour(gen_server).

-include("include/states.hrl").

-export([start_link/1]).

-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        terminate/2, 
        code_change/3]).

-define(ROUNDS, 20).	% Количество циклов проверки в тесте

start_link(Args) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

init(State) ->
	% выполнить подписку на события канала (очереди), см. eredis_smart_sub,
	% сообщения получать в handle_info по шаблону {message, <<"take_number">>}
	% события генерируются по PUBLISH в модуле random_generator
	% после отправления нового случайного числа в очередь
    gen_server:cast(State#st_pf.redis_sub, {subscribe, [State#st_pf.queue], ?SERVER}),
    {ok, State}.

handle_call(_Request, _From, State) ->	
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% сообщения получать в handle_info по шаблону {message, <<"take_number">>}
handle_info({message, <<"take_number">>}, State) ->
    % забрать из очереди число
	{ok, Bin} = eredis:q(State#st_pf.redis, ["RPOP", State#st_pf.queue]),
    Number = list_to_integer(binary_to_list(Bin)),
	% проверить число на простоту
	case is_prime(Number) of
        true -> 
				% если простое - положить в SET           
            	eredis:q(State#st_pf.redis, ["SADD", State#st_pf.set, Number]);
        _    -> no_op
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
	% перед отключением отменить подписку на события очереди
    gen_server:cast(_State#st_pf.redis_sub, {unsubscribe, [_State#st_pf.queue], ?SERVER}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ========================================================================
%% Internal functions
%% ========================================================================

%%============================================================================
%% @doc
%% Проверка числа на простоту вероятностным тестом Миллера - Рабина
%%
%% Ввод: 
%% n > 2, нечётное натуральное число, проверяемое на простоту
%% k — количество раундов проверки.
%% Вероятность ошибки теста: 1/(4^k).
%% Вывод: 
%% "составное", означает, что n является составным числом;
%% "вероятно простое", означает, что n с высокой вероятностью простое.
%% АЛГОРИТМ:
%% Представить n - 1 в виде 2^s•t, где t нечётно 
%% ЦИКЛ А: повторить k раз:
%% 	Выбрать случайное целое число а в отрезке [2, n - 2]
%% 	в х положить a^t mod n, с помощью алгоритма возведения в степень по модулю
%% 	если: х = 1 или х = n - 1, то: перейти на следующую итерацию ЦИКЛа А
%% 	ЦИКЛ В: повторить s - 1 раз
%% 		х <- х2 mod n
%% 		если: х = 1, то: вернуть "составное"
%% 		если: х = n - 1, то: перейти на следующую итерацию ЦИКЛа А
%% 	вернуть "составное"
%% вернуть "вероятно простое"
%% @end
%%============================================================================
-spec is_prime(N :: integer()) -> boolean().
is_prime(N) -> 
	if
		N ==  2		 -> true;
		N ==  3		 -> true;
		N rem 2 == 0 -> false;
		true		 ->
			{S, T} = p2_odd(N - 1),
			cycle_a(?ROUNDS, N, S, T)
	end.

	cycle_a(0      , _, _, _) -> true;
	cycle_a(Rounds , N, S, T) ->
		A = 2 + rand:uniform(N-4),
		X = power_bin_mod(A, T, N),
		if 	
			(X == 1) or (X == N - 1) -> cycle_a(Rounds - 1, N, S, T); 
			true ->	
				Res = cycle_b(S - 1, N, X),
				if 	
					Res == false -> false;
					true 		 ->	cycle_a(Rounds - 1, N, S, T)
				end
		end.
	
		cycle_b(0, _, _) -> false;
		cycle_b(Count, N, X) ->
			X2 = power_bin_mod(X, 2, N),
			if 
				X2 == 1 	-> false;
				X2 == N - 1 -> continue;
				true 		->	cycle_b(Count - 1, N, X2)
			end.
%%=============================================================================
%% @doc
%% Функция разложения числа N на произведение степени 2-ки и нечётного числа
%% @end
%%=============================================================================
-spec p2_odd(N) -> {Power, Odd} when 
	N :: integer(), Power :: integer(), Odd :: integer().
p2_odd(N)-> p2_odd(N, 0, 0).
	
p2_odd(0, Power, Odd) -> {Power, Odd};

p2_odd(N, Power, Odd) ->
	case (N rem 2) == 0 of	
		true  -> p2_odd(N div 2, Power+1, Odd);
		false -> p2_odd(0	   , Power  , N  )
	end.

%%=============================================================================
%% @docs
%% Функция бинарного возведения в степень по модулю
%% @end
%%=============================================================================
-spec power_bin_mod(Base, Power, Modulus) -> Result when
	Base :: integer(), Power :: integer(), Modulus :: integer(), Result :: integer().
power_bin_mod(Base, Power, Modulus) -> power_bin_mod(Base, Power, Modulus, 1).

power_bin_mod(_, 0, _, Result) -> Result;

power_bin_mod(Base, Power, Modulus, Result) ->
	case (Power rem 2) == 0 of
		false -> power_bin_mod(Base*Base rem Modulus, Power bsr 1, Modulus, Result*Base rem Modulus);
		true  -> power_bin_mod(Base*Base rem Modulus, Power bsr 1, Modulus, Result                 )
	end.

%%=============================================================================
%% Функция разложения числа N на произведение степени 2-ки и нечётного числа
%%=============================================================================
%p2_odd_bit(N)->	p2_odd_bit(0, N).
%
%p2_odd_bit(Power, N) when (N band 1) /= 0 -> {Power, N};
%
%p2_odd_bit(Power, N) -> p2_odd_bit(Power + 1, N bsr 1).

%%=============================================================================
%% Функция бинарного возведения в степень 
%%=============================================================================
%power_bin(Base, Power) -> power_bin(Base, Power, 1).
%
%power_bin(_, 0, Result) -> Result;
%
%power_bin(Base, Power, Result) ->
%	case (Power rem 2) == 0 of
%		false -> power_bin(Base*Base, Power bsr 1, Result*Base);
%		true  -> power_bin(Base*Base, Power bsr 1, Result     )
%	end.
%%=============================================================================	