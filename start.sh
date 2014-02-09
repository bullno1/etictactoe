erl -boot start_sasl -pa `pwd`/deps/*/ebin `pwd`/ebin -eval "application:ensure_all_started(etictactoe), application:ensure_all_started(sync)"
