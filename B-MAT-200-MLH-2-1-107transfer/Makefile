##
## EPITECH PROJECT, 2024
## Makefile
## File description:
## make bombyx
##

all: 107transfer

.PHONY: all clean fclean re tests_run cargo_build

target/release/107transfer.d: | cargo_build

include target/release/107transfer.d

cargo_build:
	cargo build --release

107transfer: target/release/107transfer
	cp target/release/107transfer .

clean:
	cargo clean

fclean: clean
	rm -f 107transfer

tests_run:
	cargo test

re:
	$(MAKE) fclean
	$(MAKE) all
