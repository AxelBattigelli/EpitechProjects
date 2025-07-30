/*
** EPITECH PROJECT, 2025
** ZappyKomJamet
** File description:
** NetworkWrapper
*/

#pragma once

#include <string>
#include <iostream>
#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>
#include <cstring>
#include <fcntl.h>

class NetworkWrapper {
    public:
        NetworkWrapper();
        ~NetworkWrapper();
        static int getaddrinfo(const char *node, const char *service, const struct addrinfo *hints, struct addrinfo **res);
        static const char *gai_strerror(int errcode);
        static int socket(int domain, int type, int protocol);
        static void perror(const char *s);
        static void freeaddrinfo(struct addrinfo *res);
        static int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
        static int close(int fd);
        static int fcntl(int fd, int cmd, ...);
        static ssize_t recv(int sockfd, void *buf, size_t len, int flags);
        static ssize_t send(int sockfd, const void *buf, size_t len, int flags);
};
