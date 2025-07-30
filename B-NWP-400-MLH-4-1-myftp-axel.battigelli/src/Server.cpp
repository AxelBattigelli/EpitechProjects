#include "Server.hpp"
#include "Exception.hpp"
#include "Client.hpp"
#include "TcpSocket.hpp"
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <format>
#include <iostream>
#include <memory>
#include <ostream>
#include <poll.h>
#include <string>
#include <sys/poll.h>
#include <utility>
#include <vector>
#include <sys/stat.h>

Server::Server(uint16_t port, std::string anonHome)
    : _listener(std::make_unique<TcpListener>(port)), _anonymousHome(std::move(anonHome))
{
    struct stat sb;

    if (stat(_anonymousHome.c_str(), &sb) != 0)
        throw Exception("Home directory doesn't exist");
    if (!this->_listener->listen()) {
        throw Exception(std::format("Cann't listening on port {}", port));
    }
    std::cout << "Server listen on port " << port << "..." << '\n';
}

void Server::start()
{
    while (true) {
        std::vector<pollfd> pollfds;
        pollfds.push_back({.fd=this->_listener->fd(),.events=POLLIN,.revents=0});
        for (const auto& client: this->_clients) {
            pollfds.push_back({.fd=client.fd(),.events=POLLIN,.revents=0});
        }
        poll(pollfds.data(), pollfds.size(), -1);
        for (const auto& polledFd : pollfds) {
            if (polledFd.revents == 0)
                continue;
            std::cout << polledFd.fd << " " << polledFd.revents <<"\n";
            if (polledFd.fd == this->_listener->fd()) {
                auto clientSocket = this->_listener->accept();
                if (clientSocket) {
                    this->_clients.push_back(std::move(Client(std::move(clientSocket), this->getPWD())));
                }
            }
            for (auto it = this->_clients.begin(); it != this->_clients.end(); ++it) {
                if (polledFd.fd == it->fd()) {
                    if (!it->ready(polledFd)) {
                        it = this->_clients.erase(it);
                        break;
                    }
                }
            }
        }
    }
}

std::string Server::getPWD()
{
    return this->_anonymousHome;
}
