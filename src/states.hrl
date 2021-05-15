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