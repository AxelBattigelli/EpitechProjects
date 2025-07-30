#pragma once

#include <cstdint>
#include <cstring>
#include <memory>
#include <netinet/in.h>
#include <string>

class TcpSocket {
  public:
    explicit TcpSocket(int socketFd);
    ~TcpSocket();
    TcpSocket(const TcpSocket&) = delete;
    TcpSocket(TcpSocket &&other) noexcept
    {
        this->socketFd = other.socketFd;
        other.socketFd = -1;
    }
    TcpSocket&operator=(const TcpSocket&) = delete;
    TcpSocket &operator=(TcpSocket &&other) noexcept
    {
        this->socketFd = other.socketFd;
        other.socketFd = -1;
        return *this;
    }
    [[nodiscard]] std::string receive() const;
    [[nodiscard]] int fd() const { return this->socketFd; };
    void send(const std::string &message) const;
    [[nodiscard]] int getSocketFd() const { return socketFd; }
    void close();

  private:
    int socketFd;
};

class TcpListener {
  public:
    explicit TcpListener(uint16_t port);
    ~TcpListener();
    TcpListener(const TcpListener&) = delete;
    TcpListener(TcpListener &&other) noexcept
    {
        this->serverSocket = other.serverSocket;
        other.serverSocket = -1;
    }
    TcpListener&operator=(const TcpListener&) = delete;
    TcpListener &operator=(TcpListener &&other) noexcept
    {
        this->serverSocket = other.serverSocket;
        other.serverSocket = -1;
        return *this;
    }
    bool listen();
    [[nodiscard]] int fd() const { return this->serverSocket; };
    [[nodiscard]] std::unique_ptr<TcpSocket> accept() const;

  private:
    int serverSocket;
    sockaddr_in serverAddr;
};
