#include "Exception.hpp"
#include "Server.hpp"
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <exception>
#include <iostream>
#include <string>

int main(int argc, char *argv[])
{
    if (argc != 3 || (argc >= 2 && (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0))) {
        std::cerr << "USAGE: ./server <port> <path>\n";
        std::cerr << "    port is the port number on which the server socket listens\n";
        std::cerr << "    path is the path to the home directory for the Anonymous user\n";
        return 0;
    }

    uint16_t const port = std::stoi(argv[1]);
    std::string const anonHome = argv[2];

    try {
        Server server(port, anonHome);
        server.start();
    } catch (const Exception &e) {
        std::cerr << "Error: " << e << '\n';
        return 84;
    } catch (const std::exception &e) {
        std::cerr << "Error: " << e.what()
                  << "\nInfo to give to the developper: " << typeid(e).name() << '\n';
        return 84;
    } catch (...) {
        std::cerr << "Unknown error" << "\nInfo to give to the developper: "
                  << std::current_exception().__cxa_exception_type()->name() << '\n';
        return 84;
    }        
    return 0;
}
