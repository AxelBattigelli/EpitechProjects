##
## EPITECH PROJECT, 2025
## Makefile
## File description:
## make stone_analysis
##

all: stone_analysis

.PHONY: all clean fclean re tests_run cargo_build

target/release/stone_analysis.d: | cargo_build

include target/release/stone_analysis.d

cargo_build:
	cargo $(CARGOFLAGS) build --release

stone_analysis: target/release/stone_analysis
	cp target/release/stone_analysis .

clean:
	cargo $(CARGOFLAGS) clean

fclean: clean
	rm -f stone_analysis

tests_run:
	cargo $(CARGOFLAGS) test

re:
	$(MAKE) fclean
	$(MAKE) all
