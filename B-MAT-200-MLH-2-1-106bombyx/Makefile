##
## EPITECH PROJECT, 2024
## Makefile
## File description:
## make bombyx
##

all: 106bombyx

.PHONY: all clean fclean re tests_run cargo_build

target/release/106bombyx.d: | cargo_build

include target/release/106bombyx.d

cargo_build:
	cargo build --release

106bombyx: target/release/106bombyx
	cp target/release/106bombyx .

clean:
	cargo clean

fclean: clean
	rm -f 106bombyx

tests_run:
	cargo test

re:
	$(MAKE) fclean
	$(MAKE) all
