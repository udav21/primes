%%%-------------------------------------------------------------------
%% @doc primes top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(primes_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%% sup_flags() = #{strategy => strategy(),         	% optional
%%                 intensity => non_neg_integer(), 	% optional
%%                 period => pos_integer()}        	% optional
%% child_spec() = #{id => child_id(),       		% mandatory
%%                  start => mfargs(),      		% mandatory
%%                  restart => restart(),   		% optional
%%                  shutdown => shutdown(), 		% optional
%%                  type => worker(),       		% optional
%%                  modules => modules()}   		% optional
init(Args) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
					#{id => primes_filter,
					start => {primes_filter, 
							  start_link,  
							  [proplists:get_value(primes_filter_args, Args)]},
					restart => permanent,
					shutdown => 1000,
					type => worker,
					modules => [primes_filter]},

					#{id => random_generator,
					start => {random_generator, 
							  start_link, 
							  [proplists:get_value(random_generator_args, Args)]},
					restart => permanent,
					shutdown => 1000,
					type => worker,
					modules => [random_generator]}
				 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
