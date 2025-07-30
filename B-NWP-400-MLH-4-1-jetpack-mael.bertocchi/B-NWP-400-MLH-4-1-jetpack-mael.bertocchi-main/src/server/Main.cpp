/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Main.cpp
*/

#include "Exception.hpp"
#include "server/GameServer.hpp"
#include <iostream>
#include <format>
#include <string.h>

int main(int argc, char **argv) {
    int port;
    std::string mapFile;
    bool debug = false;

    if (argc < 5 || argc > 6 || argv[1] == std::string("-h") || argv[1] == std::string("--help")) {
        std::cerr << "Usage: " << argv[0] << " -p <port> -m <map>\n";
        return 0;
    }

    for (int i = 1; i < argc; ++i) {
        if (std::string(argv[i]) == "-p" && i + 1 < argc) {
            port = std::stoi(argv[++i]);
        } else if (std::string(argv[i]) == "-m" && i + 1 < argc) {
            mapFile = argv[++i];
        } else if (std::string(argv[i]) == "-d") {
            debug = true;
        } else {
            std::cerr << "Usage: " << argv[0] << " -p <port> -m <map>\n";
            return 0;
        }
    }

    try {
        Jetpack::Server::GameServer server;
        server.init_server(port, mapFile, debug);
        server.accept_clients();
    } catch (const Exception &e) {
        std::cerr << "Error: " << e << '\n';
        return 84;
    } catch (const std::exception &e) {
        std::cerr << "Error: " << e.what()
                  << "\nInfo to give to the developper: " << typeid(e).name() << '\n';
        return 84;
    } catch (...) {
#if defined(__GNUG__)
#else
        std::cerr << "Unknown error (platform-specific exception type info not available)\n";
#endif
        return 84;
    }
    return 0;
}
