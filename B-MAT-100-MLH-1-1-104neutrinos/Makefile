all: 104neutrinos

.PHONY: all clean fclean re tests_run cargo_build

target/release/104neutrinos: | cargo_build

cargo_build:
	cargo build --release

104neutrinos: target/release/104neutrinos
	cp target/release/104neutrinos .

clean:
	cargo clean

fclean: clean
	rm -f 104neutrinos

tests_run:
	cargo test

re:
	$(MAKE) fclean
	$(MAKE) all
