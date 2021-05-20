cd test
erlc primes_tests.erl
cd ..
erl -noshell -pa ./test -eval "eunit:test(primes_tests, [verbose])" -s init stop