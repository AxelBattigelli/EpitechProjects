##
## EPITECH PROJECT, 2025
## nanotekspice
## File description:
## Builds stuff
##

# Rebuild all when Makefile modified
.EXTRA_PREREQS := $(abspath $(lastword $(MAKEFILE_LIST)))

SRC = $(shell find src -name '*.cpp')
HEADERS = $(shell find include -name '*.hpp')

OBJ = $(SRC:.cpp=.o)

LDLIBS = 

NAME = nanotekspice

SANFLAGS =
# SANFLAGS = -fsanitize=address,undefined -fno-omit-frame-pointer -fno-common \
	-fno-ipa-icf -fno-optimize-sibling-calls -fno-sanitize-recover=all

LDFLAGS = -Wall -Wextra -Werror -std=c++20 -g3 \
	$(SANFLAGS)
CPPFLAGS = -Wall -Wextra -Werror -iquote include
CXXFLAGS = -g3 -std=c++20 \
	$(SANFLAGS)

CLANGTIDYFLAGS =
CLANGTIDYPOSTFLAGS = -iquote include -std=c++20

.PHONY: all clean fclean re tidy tidyfix fmt tests_bless tests_run

all: $(NAME)
	# @$(MAKE) --no-print-directory -j$(shell nproc) $(NAME)

$(NAME): $(OBJ)
	$(CXX) $(LDFLAGS) -o $@ $^ $(LDLIBS) $(LOADLIBES)

%.d: %.cpp
	@set -e; rm -f $@; \
		$(CPP) -M -MP -MQ $(patsubst %.cpp,%.o,$<) -MQ \
		$(patsubst %.cpp,%.d,$<) $(CPPFLAGS) $< > $@;

include $(OBJ:.o=.d)

clean:
	find -type f -name '*.d' -delete -o -type f -name '*.o' -delete
	find -type f -name '*.gc*' -delete

fclean: clean
	find -type f -name '$(NAME)' -delete

tidy: $(SRC) $(HEADERS)
	clang-tidy $(CLANGTIDYFLAGS) $^ -- $(CLANGTIDYPOSTFLAGS)
tidyfix: CLANGTIDYFLAGS:=$(CLANGTIDYFLAGS) -fix
tidyfix: tidy

fmt: $(SRC) $(HEADERS)
	clang-format -i $^

tests_run: CXXFLAGS+=-coverage -O3
tests_run: LDFLAGS+=-coverage
tests_run: fclean .WAIT nanotekspice
	cd test && ./checker.sh
	gcovr

tests_bless: CXXFLAGS+=-coverage -O3
tests_bless: LDFLAGS+=-coverage
tests_bless: fclean .WAIT nanotekspice
	cd test && ./checker.sh --bless

re:
	$(MAKE) fclean
	$(MAKE) all
