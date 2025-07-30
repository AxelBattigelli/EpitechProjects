/*
** EPITECH PROJECT, 2025
** ZappyKomJamet
** File description:
** NetworkWrapper
*/

#include "NetworkWrapper.hpp"
#include <stdarg.h>


extern "C" {
    NetworkWrapper::NetworkWrapper()
    {
    }

    NetworkWrapper::~NetworkWrapper()
    {
    }

    int NetworkWrapper::getaddrinfo(const char *node, const char *service, const struct addrinfo *hints, struct addrinfo **res)
    {
        return ::getaddrinfo(node, service, hints, res);
    }

    const char *NetworkWrapper::gai_strerror(int errcode)
    {
        return ::gai_strerror(errcode);
    }

    int NetworkWrapper::socket(int domain, int type, int protocol)
    {
        return ::socket(domain, type, protocol);
    }

    void NetworkWrapper::perror(const char *s)
    {
        ::perror(s);
    }

    void NetworkWrapper::freeaddrinfo(struct addrinfo *res)
    {
        ::freeaddrinfo(res);
    }

    int NetworkWrapper::connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen)
    {
        return ::connect(sockfd, addr, addrlen);
    }

    int NetworkWrapper::close(int fd)
    {
        return ::close(fd);
    }

    int NetworkWrapper::fcntl(int fd, int cmd, ...)
    {
        va_list args;
        va_start(args, cmd);
        int result = ::fcntl(fd, cmd, args);
        va_end(args);
        return result;
    }

    ssize_t NetworkWrapper::recv(int sockfd, void *buf, size_t len, int flags)
    {
        return ::recv(sockfd, buf, len, flags);
    }

    ssize_t NetworkWrapper::send(int sockfd, const void *buf, size_t len, int flags)
    {
        return ::send(sockfd, buf, len, flags);
    }
}