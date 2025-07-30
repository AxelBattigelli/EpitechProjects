##
## EPITECH PROJECT, 2025
## Makefile
## File description:
## make game_of_stones
##

all: game_of_stones

.PHONY: all clean fclean re tests_run cargo_build

target/release/game_of_stones.d: | cargo_build

include target/release/game_of_stones.d

cargo_build:
	cargo build --release

game_of_stones: target/release/game_of_stones
	cp target/release/game_of_stones .

clean:
	cargo clean

fclean: clean
	rm -f game_of_stones

tests_run:
	cargo test

re:
	$(MAKE) fclean
	$(MAKE) all
