#pragma once

#include "Client.hpp"
#include "TcpSocket.hpp"
#include <csignal>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <string>
#include <vector>

class Server {
  public:
    Server(uint16_t port, std::string anonHome);
    void start();
    std::string getPWD();

  private:
    void acceptClient();
    std::unique_ptr<TcpListener> _listener;
    std::string _anonymousHome;
    std::vector<Client> _clients;
};
