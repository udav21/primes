# Primes
Application generates randoms at rate 3000 per second and filters primes from them.

>To start primes application right you must have the Redis already installed on your system, also
install rebar3, then git clone this repo, go to primes dir.

    /primes>rebar3 get-deps
    /primes>rebar3 compile
    /primes>rebar3 shell

    1>applicatio:start(primes).

## Task

Here goes practical task:

### Task Description 

It's neccessary to create application that generates equably dispenced random numbers from 2 to N (configurable parameter) and 
tests them for primality. 
 
Application have to consist of generator and filtrator (the method of realization is by your own).  
 
After apllication's start generator comes to generate random number with rate of exactly 3000 numbers per second and puts them into the queue in Redis (in structure like List). Adhering rate of 3000 is so much the better. 
 
Filtrator after the application start immediately comes to receive numbers from the queue, to check them for primality and to add prime numbers into Set structure in Redis. 

### Task Parameters 
Application parameters (the reception method is by your own): 
* N — см. выше; 
* RedisHost, RedisPort, RedisDB — parameters for the Redis connection; 
* QueueKey — key name of the queue in Redis, where the queue used by generator abides; 
* ResultSetKey — key name in  Redis, where the set used to store the filtarator results abides. 
 
### Task Requirements 
 
* Ability to start on latest major version of Erlang/OTP; 
* OTP usage; 
* Tests presence. 

