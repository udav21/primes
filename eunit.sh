#!/bin/bash
cd test
erlc app_tests.erl
cd ..
erl -noshell -pa test -eval "eunit:test(app_tests, [verbose])" -s init stop
