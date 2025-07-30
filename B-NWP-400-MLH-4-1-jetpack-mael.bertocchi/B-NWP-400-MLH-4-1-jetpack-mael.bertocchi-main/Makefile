##
## EPITECH PROJECT, 2025
## Jetpack
## File description:
## Makefile
##

SRC_CLIENT = $(shell find src/Client/ src/Shared/ -type f -name "*.cpp")
OBJ_CLIENT = $(SRC_CLIENT:.cpp=.o)

SRC_SERVER = $(shell find src/server/ src/Shared/ -type f -name '*.cpp')
OBJ_SERVER = $(SRC_SERVER:.cpp=.o)

CXXFLAGS += -std=c++20 -g -Wall -Wextra -Wpedantic -Iinclude

CLIENT = jetpack_client
SERVER = jetpack_server

all: client server

$(CLIENT): LDLIBS += -lsfml-graphics -lsfml-window -lsfml-system -lsfml-audio
$(CLIENT): $(OBJ_CLIENT)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^ $(LDLIBS)

$(SERVER): $(OBJ_SERVER)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^

client: $(CLIENT)

server: $(SERVER)

clean:
	find src -type f -name '*.o' -delete

fclean: clean
	$(RM) $(CLIENT) $(SERVER)

re: fclean all

.PHONY: all client server clean fclean re
