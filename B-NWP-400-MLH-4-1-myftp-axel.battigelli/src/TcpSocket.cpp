#include "TcpSocket.hpp"
#include "Exception.hpp"
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <memory>
#include <netinet/in.h>
#include <string>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

TcpSocket::TcpSocket(int socketFd) : socketFd(socketFd)
{
}

TcpSocket::~TcpSocket()
{
    if (socketFd != -1) {
        shutdown(socketFd, SHUT_RDWR);
        ::close(socketFd);
    }
}

std::string TcpSocket::receive() const
{
    char buffer[1024] = {0};
    ssize_t const bytesRead = read(socketFd, buffer, sizeof(buffer) - 1);

    if (bytesRead <= 0) {
        return "";
    }

    return std::string(buffer, bytesRead);
}

void TcpSocket::close() {
    if (socketFd != -1) {
        shutdown(socketFd, SHUT_RDWR);
        ::close(socketFd);
    }
    this->socketFd = -1;
}

void TcpSocket::send(const std::string &message) const
{
    ::write(socketFd, message.c_str(), message.size());
}

TcpListener::TcpListener(uint16_t port)
{
    serverSocket = socket(AF_INET, SOCK_STREAM, 0);
    if (serverSocket == -1) {
        throw Exception("Fail to create socket");
    }
    serverAddr.sin_family = AF_INET;
    serverAddr.sin_addr.s_addr = INADDR_ANY;
    serverAddr.sin_port = htons(port);
}

TcpListener::~TcpListener()
{
    if (serverSocket != -1) {
        close(serverSocket);
    }
}

bool TcpListener::listen()
{
    int yes = 1;
    if ( setsockopt(this->serverSocket, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) == -1 ) {
        throw Exception("Error with setsockopt");
    }
    if (bind(serverSocket, (struct sockaddr *) &serverAddr, sizeof(serverAddr)) == -1) {
        throw Exception("Error with bind");
    }
    if (::listen(serverSocket, 5) < 0) {
        perror("Error during listen");
        return false;
    }
    return true;
}

std::unique_ptr<TcpSocket> TcpListener::accept() const
{
    sockaddr_in clientAddr;
    socklen_t clientLen = sizeof(clientAddr);
    int const clientSocket = ::accept(serverSocket, (struct sockaddr *) &clientAddr, &clientLen);

    if (clientSocket < 0) {
        perror("Cann't accept client");
        return nullptr;
    }
    return std::make_unique<TcpSocket>(clientSocket);
}
