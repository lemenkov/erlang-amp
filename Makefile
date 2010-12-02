all:
	erlc -o ebin src/amp.erl

clean:
	rm -f src/*~ ebin/*.beam *~
