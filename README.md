# primes
Application generates randoms at rate 3000 per second and filters primes from them.


To start primes application right you should have the Redis already installed on your system, also
install rebar3, then git clone this repo, go to primes dir.
rebar3 get-deps
rebar3 compile
rebar3 shell
applicatio:start(primes)
