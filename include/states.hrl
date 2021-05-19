-record(st_rg,  {                    
                    number     = 1000000000  :: integer(),
                    queue      = "randoms"   :: string(),
                    redis                    :: pid(),
                    ticker                   :: pid()
                }).

-record(st_pf,  {   
                    queue      = "randoms"   :: string(),                 
                    set        = "primes"    :: string(),
                    redis                    :: pid(),
                    redis_sub                :: pid()
                }).
				
%% Структура для тестов
-record(args,{
                rds_host = "127.0.0.1",% IPv4
                rds_port = 6379,% 0..65535
                rds_db   = 0,% 0..15
                rds_q    = "randoms",
                rds_s    = "primes",
	            num		 = 1000000000,% int >= 2
                rds, % redis pid
                rds_sub % redis sub
             }).