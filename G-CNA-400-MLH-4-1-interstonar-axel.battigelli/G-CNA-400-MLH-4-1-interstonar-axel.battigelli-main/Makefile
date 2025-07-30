##
## EPITECH PROJECT, 2025
## Makefile
## File description:
## make interstonar
##

all: interstonar

CARGOFLAGS = --config src/use-vendored.toml

.PHONY: all clean fclean re tests_run cargo_build

target/release/interstonar.d: | cargo_build

include target/release/interstonar.d

cargo_build:
	cargo $(CARGOFLAGS) build --release

interstonar: target/release/interstonar
	cp target/release/interstonar .

clean:
	cargo $(CARGOFLAGS) clean

fclean: clean
	rm -f interstonar

tests_run:
	cargo $(CARGOFLAGS) test

vendor:
	cargo vendor --versioned-dirs

re:
	$(MAKE) fclean
	$(MAKE) all
