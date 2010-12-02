all:
	erlc +debug_info -o ebin src/amp.erl

clean:
	rm -f src/*~ ebin/*.beam *~
