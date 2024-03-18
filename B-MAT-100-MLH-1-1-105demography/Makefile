all: 105demography

.PHONY: all clean fclean re tests_run cargo_build

target/release/105demography.d: | cargo_build

include target/release/105demography.d

cargo_build:
	cargo build --release

105demography: target/release/105demography
	cp target/release/105demography .

clean:
	cargo clean

fclean: clean
	rm -f 105demography

tests_run:
	cargo test

re:
	$(MAKE) fclean
	$(MAKE) all
