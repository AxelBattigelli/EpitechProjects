all: 103cipher

.PHONY: all clean fclean re tests_run cargo_build

target/release/103cipher: | cargo_build

cargo_build:
	cargo build --release

103cipher: target/release/103cipher
	cp target/release/103cipher .

clean:
	cargo clean

fclean: clean
	rm -f 103cipher

tests_run:
	cargo test

re:
	$(MAKE) fclean
	$(MAKE) all
