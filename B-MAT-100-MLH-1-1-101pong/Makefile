all: 101pong

.PHONY: all clean fclean re 101pong

101pong: 
	cargo build --release
	cp target/release/101pong .

clean:
	cargo clean

fclean: clean
	rm -f 101pong

re:
	$(MAKE) fclean
	$(MAKE) all
