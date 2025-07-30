/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Main.cpp
*/

#include "Client/Logic/Transceiver.hpp"
#include "Client/Logic/Game.hpp"
#include "Shared/Exception.hpp"
#include "Shared/Logger.hpp"
#include "Shared/Utils.hpp"

#include <iostream>
#include <thread>

static void StartGame(const std::string& address, const std::uint16_t port, bool isDebugEnabled)
{
    std::shared_ptr<Jetpack::Client::Database> database = std::make_shared<Jetpack::Client::Database>(isDebugEnabled);
    Jetpack::Client::Transceiver transceiver(database, address, port);
    Jetpack::Client::Game game(database);
    std::thread transceiverThread([&transceiver]() {
        try {
            transceiver.Run();
        } catch (const Jetpack::Shared::Exception& ex) {
            std::cerr << "\033[31mFatal:\033[0m " << ex.what() << std::endl;
            std::exit(84);
        }
    });

    game.Run();
    transceiver.Stop();
    transceiverThread.join();
}

std::int32_t main(std::int32_t argc, char **argv)
{
    try {
        const std::string addressOption = Jetpack::Shared::Utils::GetTextOption(argv, argv + argc, "-h");
        const std::string portOption = Jetpack::Shared::Utils::GetTextOption(argv, argv + argc, "-p");
        bool isDebugEnabled = Jetpack::Shared::Utils::DoesOptionExist(argv, argv + argc, "-d");
        std::uint16_t port = static_cast<std::uint16_t>(std::stoi(portOption));

        if (!port) {
            throw Jetpack::Shared::Exception("Invalid port number.");
        }
        Jetpack::Shared::Logger::Info("Debug mode enabled", isDebugEnabled);
        StartGame(addressOption, port, isDebugEnabled);
    } catch (const std::exception& ex) {
        std::cerr << "\033[31mError:\033[0m " << ex.what() << std::endl;
        return 84;
    } catch (...) {
        std::cerr << "\033[31mError:\033[0m Unknown error occurred." << std::endl;
        return 84;
    }
    return 0;
}
