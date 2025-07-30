/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Utils
*/

#pragma once

    #include "../Wrappers/Raylib/RaylibWrapper.hpp"
    #include <string.h>
    #include <iostream>
    #include "../CameraController/CameraController.hpp"
    #include "../Map/Map.hpp"
    #include "../Particles/Particles.hpp"
    #include "../ProtocolHandler/ProtocolHandler.hpp"
    #include "../NetworkClient/NetworkClient.hpp"
    #include "../Event/Event.hpp"
    #include "../Player/Player.hpp"
    #include "../ShockWaves/ShockWaves.hpp"
    #include "../Egg/Egg.hpp"

class Utils {
    public:
        Utils();
        ~Utils();
        void LoadingScreen();
        int parseArgs(int argc, char **argv, int &port, std::string &machine);

        bool ConnectToServer(NetworkClient &client);
        void InitWindowAndSettings();
        void MainLoop(CameraController &cameraController, Map &map, ParticlesSystem &particles, ProtocolHandler &handler, NetworkClient &client, Player &players, Egg eggs, bool debugMode , bool zappykomjamet);
        int mainMenu();
        int ParametersMenu(Map &map, NetworkClient &client);
        void DrawResourcePanel(const Map &map);
        void DrawTeamInventories(const Player &players);
        bool GetTileUnderCrosshair(const Camera &camera, const Map &map, int &tileX, int &tileY);
        void DrawTileInfo(CameraController &cameraController, const Map &map);
        void DrawTeamPanel(const Player &players);
        void InitResources();
        void UnloadResources();
        void SetupSkybox();
        bool debugMode;
        bool zappykomjamet;


        struct ResourceInfo {
            const char *name;
            size_t count;
            Color color;
        };
        struct InfoLine {
            const char *label;
            size_t value;
            Color color;
        };

        struct Team {
            std::string name;
            int alive;
            int dead;
        };

        struct KeyBindings {
            int forward = KEY_W;
            int backward = KEY_S;
            int left = KEY_A;
            int right = KEY_D;
        };
        std::vector<Team> teams;
    private:
        struct KeyBindings _keyBindings;
        Font _cocFont;
        std::vector<std::string> _teamNames;
        int _selectedTeamIndex = 0;
        bool _showResourcePanel;
        bool _showTeamInventories;
        bool _showTileInfo;
        bool _showTeamPanel;
        Mesh _cube;
        Model _skyboxModel;
        Shader _skyboxShader;
        Image _skyboxImage;
        std::vector<ShockWaves> _broadCasts;

};

