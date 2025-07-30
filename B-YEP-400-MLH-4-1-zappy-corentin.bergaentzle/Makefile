##
## EPITECH PROJECT, 2025
## zappy
## File description:
## Makefile
##

CFLAGS += -std=c2x -g -Wall -Wextra -Wpedantic -Iinclude -lm -ISERVER/include
CXXFLAGS += -g -Wall -Wextra -Wpedantic -Iinclude -lraylib -lGL -lm -lpthread -ldl -lrt -lX11 -Ofast
CRITERION_FLAGS = -lcriterion

SRC_GUI := $(shell find GUI/src/ -type f -name "*.cpp" ! -name "main.cpp")
GUI_MAIN := GUI/src/main.cpp
OBJ_GUI := $(SRC_GUI:.cpp=.o)
OBJ_GUI_MAIN := $(GUI_MAIN:.cpp=.o)

SRC_SERVER := $(shell find SERVER/src/ -type f -name "*.c" ! -name "main.c")
OBJ_SERVER := $(SRC_SERVER:.c=.o)
SERVER_MAIN := SERVER/src/main.c
OBJ_SERVER_MAIN := $(SERVER_MAIN:.c=.o)

SRC_AI := AI/src/main.py

AI := zappy_ai
GUI := zappy_gui
SERVER := zappy_server

TESTS_GUI := $(shell find tests/gui/ -type f -name "*.cpp")
TESTS_SERVER := $(shell find tests/server/ -type f -name "*.c")

UNIT_TEST_BIN_GUI := unit_tests_gui
UNIT_TEST_BIN_SERVER := unit_tests_server

all: zappy_ai zappy_gui zappy_server

$(GUI): $(OBJ_GUI) $(OBJ_GUI_MAIN)
	$(CXX) $(LDFLAGS) -o $@ $^ $(LDLIBS) -g -Wall -Wextra -Wpedantic -Iinclude -lraylib -lGL -lm -lpthread -ldl -lrt -lX11 -Ofast

$(SERVER): $(OBJ_SERVER) $(OBJ_SERVER_MAIN)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^ $(LDLIBS)

zappy_gui: $(GUI)

zappy_server: $(SERVER)

zappy_ai:
	ln -sf $(SRC_AI) $(AI)

clean:
	find GUI/src -type f -name '*.o' -delete
	find SERVER/src -type f -name '*.o' -delete

fclean: clean
	$(RM) -rf $(AI) $(GUI) $(SERVER) $(UNIT_TEST_BIN_GUI) $(UNIT_TEST_BIN_SERVER) *.html *.xml htmlcov *.css
	rm -rf *.gcda
	rm -rf *.gcno

re: fclean all

unit_test_gui: $(SRC_GUI) $(TESTS_GUI)
	$(CXX) -o $(UNIT_TEST_BIN_GUI) $^ -g -Wall -Wextra -Wpedantic -Iinclude -lraylib -lGL -lm -lpthread -ldl -lrt -lX11 -Ofast $(CRITERION_FLAGS) $(COVERAGE_FLAGS) --coverage

unit_test_server: $(SRC_SERVER) $(TESTS_SERVER)
	$(CC) -o $(UNIT_TEST_BIN_SERVER) $^ $(CFLAGS) $(CRITERION_FLAGS)
		$(COVERAGE_FLAGS)

tests_run: unit_test_gui unit_test_server
	./$(UNIT_TEST_BIN_GUI)
	./$(UNIT_TEST_BIN_SERVER)

.PHONY: all ai gui server clean fclean re
