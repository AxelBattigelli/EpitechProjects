all: 102architect

.PHONY: all clean fclean re tests_run cargo_build

target/release/102architect: | cargo_build

cargo_build:
	cargo build --release

102architect: target/release/102architect
	cp target/release/102architect .

clean:
	cargo clean

fclean: clean
	rm -f 102architect

tests_run:
	cargo test

re:
	$(MAKE) fclean
	$(MAKE) all
