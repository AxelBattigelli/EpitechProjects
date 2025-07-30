##
## EPITECH PROJECT, 2025
## PDGRUSH3
## File description:
## Builds stuff
##

# Rebuild all when Makefile modified
.EXTRA_PREREQS := $(abspath $(lastword $(MAKEFILE_LIST)))

SRC = 	./src/strlen.asm	\
		./src/strchr.asm	\
		./src/strrchr.asm	\
		./src/memset.asm	\
		./src/memcpy.asm	\
		./src/strcmp.asm	\
		./src/strncmp.asm	\
		./src/strcasecmp.asm	\
		./src/memmove.asm	\
		./src/strpbrk.asm	\
		./src/strstr.asm	\

OBJ = $(SRC:.asm=.o)

LDLIBS =

NAME = libasm.so

SANFLAGS =
# SANFLAGS = -fsanitize=address,undefined -fno-omit-frame-pointer -fno-common \
	-fno-ipa-icf -fno-optimize-sibling-calls -fno-sanitize-recover=all

LDFLAGS = -shared

ASFLAGS = -g -f elf64

AS = nasm

.PHONY: all clean fclean re

all:
	@$(MAKE) --no-print-directory -j$(shell nproc) $(NAME)

$(NAME): $(OBJ)
	$(LD) $(LDFLAGS) -o $@ $^ $(LDLIBS) $(LOADLIBES)

%.o: %.asm
	$(AS) $(ASFLAGS) -o $@ $^

clean:
	find -type f -name '*.d' -delete -o -type f -name '*.o' -delete
	find -type f -name '*.gc*' -delete

fclean: clean
	find -type f -name '$(NAME)' -delete

re:
	$(MAKE) fclean
	$(MAKE) all