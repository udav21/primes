cd test
erlc primes_tests.erl
erl -noshell -eval "eunit:test(primes_tests, [verbose])" -s init stop
cd ..