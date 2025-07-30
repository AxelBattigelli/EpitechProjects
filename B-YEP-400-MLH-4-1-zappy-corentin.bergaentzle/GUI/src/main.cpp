/*
** EPITECH PROJECT, 2025
** gui
** File description:
** main
*/

#include "Wrappers/Raylib/RaylibWrapper.hpp"
#include "CameraController/CameraController.hpp"
#include "Map/Map.hpp"
#include "Utils/Utils.hpp"
#include "Particles/Particles.hpp"
#include "Event/Event.hpp"
#include "NetworkClient/NetworkClient.hpp"
#include "ProtocolHandler/ProtocolHandler.hpp"
#include "Player/Player.hpp"
#include "Utils/Utils.hpp"
#include <iostream>
#include <cstring>

int main(int argc, char **argv)
{
    int port = 0;
    bool debugMode = false;
    bool zappykomjamet = false;
    std::string machine;
    Utils utils;
    int value;
    value = utils.parseArgs(argc, argv, port, machine);
    if (value == 0) {
        return 84;
    }
    if (utils.debugMode) {
        debugMode = true;
    }
    if (utils.zappykomjamet) {
        zappykomjamet = true;
    }

    utils.InitWindowAndSettings();
    utils.InitResources();

    CameraController cameraController;
    Map map(10, 10);
    Player players;
    Egg eggs;
    ParticlesSystem particles;
    Event event(cameraController, particles);
    ProtocolHandler handler(map, players, eggs, particles);

    NetworkClient client(machine, port);

    if (!utils.ConnectToServer(client))
        return 84;

    utils.LoadingScreen();
    int menuChoice = utils.mainMenu();
    if (menuChoice == 1) {
        int paramChoice = utils.ParametersMenu(map, client);
        if (paramChoice == 2) {
            utils.mainMenu();
        }
    }
    if (menuChoice == 2) {
        utils.UnloadResources();
        RaylibWrapper::CloseWindow();
        return 0;
    }
    utils.MainLoop(cameraController, map, particles, handler, client, players, eggs, debugMode, zappykomjamet);

    utils.UnloadResources();
    RaylibWrapper::CloseWindow();
    return 0;
}
