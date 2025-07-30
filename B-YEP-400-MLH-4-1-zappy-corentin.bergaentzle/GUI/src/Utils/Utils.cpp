/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Utils
*/

#include "Utils.hpp"
#include <vector>
#include <string>
#include <thread>
#include <future>
#include <raymath.h>
#include <fcntl.h>

Utils::Utils()
{
    _showResourcePanel = true;
    _showTeamInventories = true;
    _showTileInfo = true;
    _showTeamPanel = true;
}

Utils::~Utils()
{}
extern "C" {
    void Utils::InitResources() {
        _cocFont = RaylibWrapper::LoadFont("GUI/assets/font/coc-font.ttf");
    }

    void Utils::UnloadResources() {
        RaylibWrapper::UnloadFont(_cocFont);
        RaylibWrapper::UnloadShader(_skyboxModel.materials[0].shader);
        RaylibWrapper::UnloadTexture(_skyboxModel.materials[0].maps[MATERIAL_MAP_CUBEMAP].texture);
        RaylibWrapper::UnloadModel(_skyboxModel);
    }

    void Utils::LoadingScreen()
    {
        RaylibWrapper::InitAudioDevice();
        Music music = RaylibWrapper::LoadMusicStream("GUI/assets/music/coc_intro.ogg");
        RaylibWrapper::PlayMusicStream(music);
        Texture2D supercellLogo = RaylibWrapper::LoadTexture("GUI/assets/backgrounds/logo_supercell.png");
        double startTime = RaylibWrapper::GetTime();
        while (RaylibWrapper::GetTime() - startTime < 1.7 && !RaylibWrapper::WindowShouldClose()) {
            RaylibWrapper::UpdateMusicStream(music);
            RaylibWrapper::BeginDrawing();
            RaylibWrapper::ClearBackground(BLACK);
            RaylibWrapper::DrawTexture(supercellLogo, 1920/2 - supercellLogo.width/2, 1080/2 - supercellLogo.height/2, WHITE);
            RaylibWrapper::EndDrawing();
        }
        RaylibWrapper::UnloadTexture(supercellLogo);

        Texture2D bg = RaylibWrapper::LoadTexture("GUI/assets/backgrounds/bg.png");


        float progress = 0.0f;
        const float progressSpeed = 0.35f;
        int barWidth = 600;
        int barHeight = 32;
        int barX = 1920/2 - barWidth/2;
        int barY = 1080 - barHeight - 100;

        Color cocPurple = { 186, 85, 211, 255 };

        const char *loadingText = "Loading";

        while (progress < 1.0f && !RaylibWrapper::WindowShouldClose()) {
            RaylibWrapper::UpdateMusicStream(music);

            RaylibWrapper::BeginDrawing();
            RaylibWrapper::ClearBackground(SKYBLUE);
            RaylibWrapper::DrawTexture(bg, 0, 0, WHITE);

            int loadingFontSize = 38;
            Vector2 loadingPos = {1920/2.0f - MeasureTextEx(_cocFont, loadingText, loadingFontSize, 2).x/2, (float)(barY - loadingFontSize)};
            RaylibWrapper::DrawTextEx(_cocFont, loadingText, loadingPos, loadingFontSize, 2, WHITE);

            Rectangle progressRect = {(float)barX, (float)barY, (float)(barWidth * progress), (float)barHeight};
            Rectangle barRect = {(float)barX, (float)barY, (float)barWidth, (float)barHeight};
            RaylibWrapper::DrawRectangleRounded(progressRect, 0.5f, 16, cocPurple);
            RaylibWrapper::DrawRectangleRoundedLines(barRect, 0.5f, 16, BLACK);
            RaylibWrapper::EndDrawing();

            progress += progressSpeed * GetFrameTime();
            if (progress > 1.0f)
                progress = 1.0f;
        }
        RaylibWrapper::StopMusicStream(music);
        RaylibWrapper::UnloadMusicStream(music);
        RaylibWrapper::CloseAudioDevice();

        RaylibWrapper::UnloadTexture(bg);
    }

    bool Utils::ConnectToServer(NetworkClient &client)
    {
        if (!client.connectToServer()) {
            std::cerr << "Failed to connect to server" << std::endl;
            return false;
        }

        int fd = client.getSocketFd();
        int flags = fcntl(fd, F_GETFL, 0);
        fcntl(fd, F_SETFL, flags & ~O_NONBLOCK);

        std::string welcome = client.readLine();

        fcntl(fd, F_SETFL, flags | O_NONBLOCK);

        std::cout << "Welcome message: " << welcome << std::endl;
        if (welcome != "WELCOME") {
            std::cout << "test Welcome" << std::endl;
        }
        client.sendLine("GRAPHIC");
        return true;
    }

    const char* GetKeyName(int key) {
        switch (key) {
            case KEY_W: return "Z";
            case KEY_Z: return "W";
            case KEY_Q: return "A";
            case KEY_A: return "Q";
            case KEY_SEMICOLON: return "M";
            case KEY_M: return ";";
            case KEY_UP: return "UP";
            case KEY_DOWN: return "DOWN";
            case KEY_LEFT: return "LEFT";
            case KEY_RIGHT: return "RIGHT";
            case KEY_SPACE: return "SPACE";
            default: return TextFormat("%c", key);
        }
    }

    int Utils::ParametersMenu(Map &map, NetworkClient &client)
    {
        std::vector<std::string> options = {
            "Show Resource Panel",
            "Show Team Inventories",
            "Show Tile Info",
            "Show Team Panel",
            "Change Key Bindings",
            "Set Time",
            "Back"
        };
        int selected = 0;
        int optionCount = options.size();
        int waitingForKey = -1;
        bool inKeybindMenu = false;
        int keybindSelected = 0;
        Texture2D SettingsBackground = RaylibWrapper::LoadTexture("GUI/assets/backgrounds/SettingsBackground.png");
        bool enteringTime = false;
        char timeInput[16] = "";
        int timeInputLen = 0;

        while (!RaylibWrapper::WindowShouldClose()) {
            RaylibWrapper::BeginDrawing();
            RaylibWrapper::ClearBackground(RAYWHITE);

            int titleFontSize = 48;
            Vector2 titlePos = {1920/2.0f - RaylibWrapper::MeasureTextEx(_cocFont, "Display Settings", titleFontSize, 2).x/2, 180};
            RaylibWrapper::DrawTextEx(_cocFont, "Display Settings", titlePos, titleFontSize, 2, BLACK);
            RaylibWrapper::DrawTexture(SettingsBackground, 0, 0, WHITE);
            if (!inKeybindMenu) {
                for (int i = 0; i < optionCount; ++i) {
                    Color color = (i == selected) ? DARKGRAY : LIGHTGRAY;
                    int y = 320 + i * 70;
                    RaylibWrapper::DrawRectangle(1920/2 - 300, y, 600, 60, color);

                    std::string label = options[i];
                    if (i == 0) label += _showResourcePanel ? " [ON]" : " [OFF]";
                    if (i == 1) label += _showTeamInventories ? " [ON]" : " [OFF]";
                    if (i == 2) label += _showTileInfo ? " [ON]" : " [OFF]";
                    if (i == 3) label += _showTeamPanel ? " [ON]" : " [OFF]";
                    if (i == 5) label += " (" + std::to_string(map._time) + ")";

                    RaylibWrapper::DrawTextEx(_cocFont, label.c_str(), {(float)(1920/2 - 270), (float)(y + 15)}, 32, 2, (i == selected) ? YELLOW : BLACK);
                }
            } else {
                RaylibWrapper::DrawTextEx(_cocFont, "Change Key Bindings", {1920/2.0f - 200, 260}, 38, 2, BLACK);

                const char* labels[4] = {"Forward", "Backward", "Left", "Right"};
                int* keyPtrs[4] = {&_keyBindings.forward, &_keyBindings.backward, &_keyBindings.left, &_keyBindings.right};

                for (int i = 0; i < 4; i++) {
                    int y = 340 + i * 60;
                    Color color = (i == keybindSelected) ? DARKGRAY : LIGHTGRAY;
                    RaylibWrapper::DrawRectangle(1920/2 - 250, y, 500, 48, color);
                    std::string txt = std::string(labels[i]) + " : " + GetKeyName(*keyPtrs[i]);
                    RaylibWrapper::DrawTextEx(_cocFont, txt.c_str(), {(float)(1920/2 - 220), (float)(y + 8)}, 32, 2, (i == keybindSelected) ? YELLOW : BLACK);
                }
                RaylibWrapper::DrawTextEx(_cocFont, "Back", {(float)(1920/2 - 60), 340 + 4 * 60 + 8}, 32, 2, keybindSelected == 4 ? YELLOW : BLACK);

                if (waitingForKey != -1) {
                    RaylibWrapper::DrawTextEx(_cocFont, "Press a key...", {1920/2.0f - 180, 700}, 32, 2, RED);
                }
            }

            if (enteringTime) {
                int boxW = 400;
                int boxH = 90;
                int boxX = 1920/2 - boxW/2;
                int boxY = 320 + optionCount * 70 + 40;

                RaylibWrapper::DrawRectangle(boxX, boxY, boxW, boxH, Fade(DARKGRAY, 0.93f));
                RaylibWrapper::DrawRectangleLines(boxX, boxY, boxW, boxH, BLACK);

                RaylibWrapper::DrawTextEx(_cocFont, "Set new game time", {(float)(boxX + 24), (float)(boxY + 10)}, 28, 2, YELLOW);

                RaylibWrapper::DrawRectangle(boxX + 20, boxY + 44, boxW - 40, 36, RAYWHITE);
                RaylibWrapper::DrawRectangleLines(boxX + 20, boxY + 44, boxW - 40, 36, BLACK);
                RaylibWrapper::DrawTextEx(_cocFont, timeInput, {(float)(boxX + 32), (float)(boxY + 48)}, 28, 2, BLACK);
            }

            RaylibWrapper::EndDrawing();

            if (!inKeybindMenu && !enteringTime) {
                if (RaylibWrapper::IsKeyPressed(KEY_DOWN)) selected = (selected + 1) % optionCount;
                if (RaylibWrapper::IsKeyPressed(KEY_UP)) selected = (selected - 1 + optionCount) % optionCount;
                if (RaylibWrapper::IsKeyPressed(KEY_ENTER) || RaylibWrapper::IsKeyPressed(KEY_SPACE)) {
                    if (selected == 0) _showResourcePanel = !_showResourcePanel;
                    else if (selected == 1) _showTeamInventories = !_showTeamInventories;
                    else if (selected == 2) _showTileInfo = !_showTileInfo;
                    else if (selected == 3) _showTeamPanel = !_showTeamPanel;
                    else if (selected == 4) { inKeybindMenu = true; keybindSelected = 0; }
                    else if (selected == 5) {
                        enteringTime = true;
                        timeInputLen = 0;
                        timeInput[0] = '\0';
                    }
                    else if (selected == 6) return selected;
                }
                if (RaylibWrapper::IsKeyPressed(KEY_ESCAPE)) return selected;
            } else if (enteringTime) {
                int key = GetKeyPressed();
                if (key >= '0' && key <= '9' && timeInputLen < 15) {
                    timeInput[timeInputLen++] = (char)key;
                    timeInput[timeInputLen] = '\0';
                }
                if (key == KEY_BACKSPACE && timeInputLen > 0) {
                    timeInput[--timeInputLen] = '\0';
                }
                if (RaylibWrapper::IsKeyPressed(KEY_ENTER) && timeInputLen > 0) {
                    int newTime = atoi(timeInput);
                    client.sendSst(newTime, debugMode);
                    enteringTime = false;
                }
                if (RaylibWrapper::IsKeyPressed(KEY_ESCAPE)) {
                    enteringTime = false;
                }
            }
        }
        return selected;
    }

    int Utils::parseArgs(int argc, char **argv, int &port, std::string &machine)
    {
        port = -1;
        machine.clear();

        for (int i = 1; i < argc; i += 2) {
            if (strcmp(argv[i], "-help") == 0 || strcmp(argv[i], "--help") == 0) {
                std::cout << "Usage: " << argv[0] << " -p port -h machine" << std::endl;
                return 0;
            }
            if (strcmp(argv[i], "-p") == 0) {
                port = std::stoi(argv[i + 1]);
            } else if (strcmp(argv[i], "-h") == 0) {
                machine = argv[i + 1];
            } else if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "-debug") == 0) {
                std::cout << "Debug mode enabled." << std::endl;
                i--;
                debugMode = true;
            } else if (strcmp(argv[i], "-ZappyKomJamet") == 0 || strcmp(argv[i], "-zapzapzap") == 0) {
                std::cout << "ZappyKomJamet mode enabled." << std::endl;
                zappykomjamet = true;
                i--;
            } else {
                std::cerr << "Unknown flag: " << argv[i] << std::endl;
                std::cerr << "Usage: " << argv[0] << " -p port -h machine" << std::endl;
                return 0;
            }
        }

        if (port == -1 || machine.empty()) {
            std::cerr << "Both -p and -h flags are required." << std::endl;
            return 0;
        }
        return 1;
    }

    void Utils::InitWindowAndSettings()
    {
        TraceLogLevel logLevel = LOG_NONE;
        RaylibWrapper::SetTraceLogLevel(logLevel);
        const int screenWidth = 1920;
        const int screenHeight = 1080;
        RaylibWrapper::InitWindow(screenWidth, screenHeight, "Zappy");
        RaylibWrapper::SetExitKey(KEY_C);
        RaylibWrapper::DisableCursor();
    }

    void Utils::SetupSkybox()
    {
        _cube = RaylibWrapper::GenMeshCube(1.0f, 1.0f, 1.0f);
        _skyboxModel = RaylibWrapper::LoadModelFromMesh(_cube);
        _skyboxModel.materials[0].shader = RaylibWrapper::LoadShader("GUI/assets/shaders/skybox.vs", "GUI/assets/shaders/skybox.fs");

        RaylibWrapper::SetShaderValue(_skyboxModel.materials[0].shader, RaylibWrapper::GetShaderLocation(_skyboxModel.materials[0].shader, "environmentMap"), MATERIAL_MAP_CUBEMAP);
        RaylibWrapper::SetShaderValue(_skyboxModel.materials[0].shader, RaylibWrapper::GetShaderLocation(_skyboxModel.materials[0].shader, "doGamma"), 0);
        RaylibWrapper::SetShaderValue(_skyboxModel.materials[0].shader, RaylibWrapper::GetShaderLocation(_skyboxModel.materials[0].shader, "vflipped"), 0);

        _skyboxShader = RaylibWrapper::LoadShader("GUI/assets/shaders/cubemap.vs", "GUI/assets/shaders/cubemap.fs");

        RaylibWrapper::SetShaderValue(_skyboxShader, RaylibWrapper::GetShaderLocation(_skyboxShader, "equirectangularMap"), 0);

        _skyboxImage = RaylibWrapper::LoadImage("GUI/assets/skybox/skybox.png");
        _skyboxModel.materials[0].maps[MATERIAL_MAP_CUBEMAP].texture = RaylibWrapper::LoadTextureCubemap(_skyboxImage, CUBEMAP_LAYOUT_AUTO_DETECT);

        RaylibWrapper::UnloadImage(_skyboxImage);
    }

    void Utils::MainLoop(CameraController &cameraController, Map &map, ParticlesSystem &particles,ProtocolHandler &handler, NetworkClient &client, Player &players, Egg eggs, bool debugMode, bool zappykomjamet)
    {
        SetupSkybox();
        RaylibWrapper::InitAudioDevice();
        Music GameMusic = LoadMusicStream("GUI/assets/music/GameMusic.ogg");
        RaylibWrapper::PlayMusicStream(GameMusic);
        while (!RaylibWrapper::WindowShouldClose()) {
            if (!RaylibWrapper::IsMusicStreamPlaying(GameMusic)) {
                RaylibWrapper::PlayMusicStream(GameMusic);
            }
            RaylibWrapper::UpdateMusicStream(GameMusic);
            std::string msg = client.readLine();
            if (!msg.empty()) {
                handler.handleMessage(msg, debugMode);
            }

            cameraController.Update();
            particles.Update(RaylibWrapper::GetFrameTime());
            players.UpdateBroadCasts(RaylibWrapper::GetFrameTime());
            players.EraseEmptyBroadCasts();
            if (RaylibWrapper::IsKeyPressed(KEY_LEFT)) {
                _selectedTeamIndex--;
            }
            if (RaylibWrapper::IsKeyPressed(KEY_RIGHT)) {
                _selectedTeamIndex++;
            }

            float moveSpeed = 0.05f;

            Camera camera = cameraController.GetCamera();

            Vector3 forward = Vector3Normalize(Vector3Subtract(camera.target, camera.position));
            Vector3 right = Vector3Normalize(Vector3CrossProduct(forward, camera.up));

            bool moved = false;
            Vector3 moveDelta = {0, 0, 0};
            if (RaylibWrapper::IsKeyDown(_keyBindings.forward)) {
                moveDelta = Vector3Add(moveDelta, Vector3Scale(forward, moveSpeed));
                moved = true;
            }
            if (RaylibWrapper::IsKeyDown(_keyBindings.backward)) {
                moveDelta = Vector3Subtract(moveDelta, Vector3Scale(forward, moveSpeed));
                moved = true;
            }
            if (RaylibWrapper::IsKeyDown(_keyBindings.left)) {
                moveDelta = Vector3Subtract(moveDelta, Vector3Scale(right, moveSpeed));
                moved = true;
            }
            if (RaylibWrapper::IsKeyDown(_keyBindings.right)) {
                moveDelta = Vector3Add(moveDelta, Vector3Scale(right, moveSpeed));
                moved = true;
            }
            if (moved) {
                camera.position = Vector3Add(camera.position, moveDelta);
                camera.target = Vector3Add(camera.target, moveDelta);
                cameraController.SetCamera(camera);
            }

            if (RaylibWrapper::IsKeyPressed(KEY_P)) {
                client.sendMsz(debugMode);
                auto selectedTile = map.getTilePositionFrom(0, 0);
                client.sendBct(selectedTile.x, selectedTile.y, debugMode);
                client.sendMct(debugMode);
                client.sendTna(debugMode);
                client.sendPpo(players.players.count(0), debugMode);
                client.sendPlv(players.players.count(0), debugMode);
                client.sendPin(players.players.count(0), debugMode);
                client.sendSgt(debugMode);
                client.sendSst(map._time,debugMode);
            }

            RaylibWrapper::BeginDrawing();
            RaylibWrapper::ClearBackground(SKYBLUE);

            RaylibWrapper::BeginMode3D(cameraController.GetCamera());

            RaylibWrapper::rlDisableBackfaceCulling();
            RaylibWrapper::rlDisableDepthMask();
            RaylibWrapper::DrawModel(_skyboxModel, {0, 0, 0}, 1.0f, WHITE);
            RaylibWrapper::rlEnableBackfaceCulling();
            RaylibWrapper::rlEnableDepthMask();

            map.DrawMap();
            players.drawPlayersOnMap(map);
            eggs.drawEggsOnMap(map);
            particles.Draw();

            players.DrawBroadCasts();
            RaylibWrapper::EndMode3D();

            int crossX = 1920 / 2;
            int crossY = 1080 / 2;
            RaylibWrapper::DrawLine(crossX - 10, crossY, crossX + 10, crossY, BLACK);
            RaylibWrapper::DrawLine(crossX, crossY - 10, crossX, crossY + 10, BLACK);
            if (RaylibWrapper::IsKeyPressed(KEY_ESCAPE)) {
                Utils::ParametersMenu(map, client);
            }
            if (_showTileInfo)
                DrawTileInfo(cameraController, map);
            if (_showResourcePanel)
                DrawResourcePanel(map);
            if (_showTeamPanel)
                DrawTeamPanel(players);
            if (_showTeamInventories)
                DrawTeamInventories(players);
            RaylibWrapper::DrawText(TextFormat("FPS: %d", GetFPS()), 10, 10, 20, YELLOW);
            RaylibWrapper::EndDrawing();
        }
    }

    void Utils::DrawResourcePanel(const Map &map)
    {
        auto foodFuture = std::async(std::launch::async, [&map](){ return map.getFoodsOnMap(); });
        auto linemateFuture = std::async(std::launch::async, [&map](){ return map.getLinematesOnMap(); });
        auto deraumereFuture = std::async(std::launch::async, [&map](){ return map.getDeraumeresOnMap(); });
        auto siburFuture = std::async(std::launch::async, [&map](){ return map.getSibursOnMap(); });
        auto mendianeFuture = std::async(std::launch::async, [&map](){ return map.getMendianesOnMap(); });
        auto phirasFuture = std::async(std::launch::async, [&map](){ return map.getPhirasOnMap(); });
        auto thystameFuture = std::async(std::launch::async, [&map](){ return map.getThystamesOnMap();});

        size_t food = foodFuture.get();
        size_t linemate = linemateFuture.get();
        size_t deraumere = deraumereFuture.get();
        size_t sibur = siburFuture.get();
        size_t mendiane = mendianeFuture.get();
        size_t phiras = phirasFuture.get();
        size_t thystame = thystameFuture.get();

        int panelWidth = 350;
        int panelX = 1920 - panelWidth - 40;
        int panelY = 80;
        int panelHeight = 320;
        RaylibWrapper::DrawRectangle(panelX, panelY, panelWidth, panelHeight, RaylibWrapper::Fade(DARKGRAY, 0.7f));
        RaylibWrapper::DrawRectangleLines(panelX, panelY, panelWidth, panelHeight, BLACK);

        int titleFontSize = 32;
        RaylibWrapper::DrawTextEx(_cocFont, "Map Resources", {(float)(panelX + 20), (float)(panelY + 18)}, titleFontSize, 2, WHITE);

        ResourceInfo resources[] = {
            {"Food", food, RED},
            {"Linemate", linemate, GOLD},
            {"Deraumere", deraumere, ORANGE},
            {"Sibur", sibur, GREEN},
            {"Mendiane", mendiane, PINK},
            {"Phiras", phiras, BLACK},
            {"Thystame", thystame, SKYBLUE}
        };

        int resFontSize = 24;
        int iconSize = 24;
        int spacing = 12;
        int y = panelY + 60;

        for (int i = 0; i < 7; ++i) {
            RaylibWrapper::DrawRectangle(panelX + 20, y, iconSize, iconSize, resources[i].color);
            RaylibWrapper::DrawTextEx(_cocFont, resources[i].name, {(float)(panelX + 60), (float)y}, resFontSize, 2, WHITE);
            RaylibWrapper::DrawTextEx(_cocFont, TextFormat("%zu", resources[i].count), {(float)(panelX + panelWidth - 60), (float)y}, resFontSize, 2, WHITE);
            y += iconSize + spacing;
        }
    }

    void Utils::DrawTeamInventories(const Player &players)
    {
        auto teamInventories = players.getTeamInventories();
        if (teamInventories.empty())
            return;

        std::vector<std::string> teamNames;
        for (const auto &kv : teamInventories)
            teamNames.push_back(kv.first);

        if (_selectedTeamIndex < 0) _selectedTeamIndex = 0;
        if (_selectedTeamIndex >= (int)teamNames.size()) _selectedTeamIndex = teamNames.size() - 1;

        const std::string &selectedTeam = teamNames[_selectedTeamIndex];
        const auto &inventories = teamInventories[selectedTeam];

        int panelWidth = 520;
        int panelX = 40;
        int rowHeight = 36;
        int titleHeight = 54;
        int padding = 18;
        int playerCount = inventories.size();
        int panelHeight = titleHeight + playerCount * rowHeight + padding * 2;
        int panelY = 1000 - panelHeight - 40;

        RaylibWrapper::DrawRectangle(panelX, panelY, panelWidth, panelHeight, Fade(DARKGRAY, 0.8f));
        RaylibWrapper::DrawRectangleLines(panelX, panelY, panelWidth, panelHeight, BLACK);

        RaylibWrapper::DrawTextEx(_cocFont, selectedTeam.c_str(), {(float)(panelX + 50), (float)(panelY + 18)}, 32, 2, WHITE);

        int y = panelY + titleHeight + 30;
        int fontSize = 20;

        Color resourceColors[7] = {
            RED,
            GOLD,
            ORANGE,
            GREEN,
            PINK,
            BLACK,
            SKYBLUE
        };

        std::vector<std::string> resourceNames = {
            "Food", "Linemate", "Deraumere", "Sibur", "Mendiane", "Phiras", "Thystame"
        };

        for (const auto &player : inventories) {
            int playerId = player.first;
            const auto &resources = player.second;
            std::string playerLabel = "Player #" + std::to_string(playerId) + " (Lvl " + std::to_string(players.getPlayerlvlbyid(playerId)) + "): ";
            RaylibWrapper::DrawTextEx(_cocFont, playerLabel.c_str(), {(float)(panelX + 24), (float)y}, fontSize, 2, GOLD);

            float x = panelX + 260;
            for (size_t i = 0; i < 7 && i < resources.size(); ++i) {
            std::string numStr = std::to_string(resources[i]);
            RaylibWrapper::DrawTextEx(_cocFont, numStr.c_str(), {x, (float)y}, fontSize, 2, resourceColors[i]);
            x += RaylibWrapper::MeasureTextEx(_cocFont, numStr.c_str(), fontSize, 2).x + 12;
            }
            y += rowHeight;
        }

        RaylibWrapper::DrawTextEx(_cocFont, "<", {(float)(panelX + 10), (float)(panelY + 18)}, 32, 2, WHITE);
        RaylibWrapper::DrawTextEx(_cocFont, ">", {(float)(panelX + panelWidth - 34), (float)(panelY + 18)}, 32, 2, WHITE);
    }

    bool Utils::GetTileUnderCrosshair(const Camera &camera, const Map &map, int &tileX, int &tileY)
    {
        Vector2 screenCenter = {1920 / 2.0f, 1080 / 2.0f};
        Ray ray = GetMouseRay(screenCenter, camera);

        float t = -ray.position.y / ray.direction.y;
        if (t < 0)
            return false;

        Vector3 hit = Vector3Add(ray.position, Vector3Scale(ray.direction, t));
        tileX = (int)roundf(hit.x);
        tileY = (int)roundf(hit.z);

        auto tilesSize = static_cast<int>(map.getTiles().size());
        if (tileX < 0 || tileY < 0 || tileX >= tilesSize || tileY >= tilesSize)
            return false;
        return true;
    }

    void Utils::DrawTileInfo(CameraController &cameraController, const Map &map)
    {
        int tileX, tileY;
        Camera cam = cameraController.GetCamera();
        if (GetTileUnderCrosshair(cam, map, tileX, tileY)) {
            auto tiles = map.getTiles();
            for (const auto &tile : tiles) {
                if (tile.coords.first == tileX && tile.coords.second == tileY) {
                    int InfoWidth = 350;
                    int InfoHeight = 350;
                    int InfoX = 40;
                    int InfoY = 80;

                    DrawRectangle(InfoX, InfoY, InfoWidth, InfoHeight, Fade(DARKGRAY, 0.7f));
                    DrawRectangleLines(InfoX, InfoY, InfoWidth, InfoHeight, BLACK);

                    int titleFontSize = 28;
                    DrawTextEx(_cocFont, TextFormat("Tile (%d, %d)", tileX, tileY), {(float)(InfoX + 20), (float)(InfoY + 18)}, titleFontSize, 2, WHITE);

                    struct InfoLine {
                        const char* label;
                        size_t value;
                        Color color;
                    };

                    InfoLine lines[] = {
                        {"Players", tile.nbPlayers, WHITE},
                        {"Food", tile.nbFood, RED},
                        {"Linemate", tile.nbLinemate, GOLD},
                        {"Deraumere", tile.nbDeraumere, ORANGE},
                        {"Sibur", tile.nbSibur, GREEN},
                        {"Mendiane", tile.nbMendiane, PINK},
                        {"Phiras", tile.nbPhiras, BLACK},
                        {"Thystame", tile.nbThystame, SKYBLUE}
                    };

                    int resFontSize = 22;
                    int iconSize = 22;
                    int spacing = 12;
                    int y = InfoY + 60;

                    for (int i = 0; i < 8; ++i) {
                        DrawRectangle(InfoX + 20, y, iconSize, iconSize, lines[i].color);
                        DrawTextEx(_cocFont, lines[i].label, {(float)(InfoX + 60), (float)y}, resFontSize, 2, WHITE);
                        DrawTextEx(_cocFont, TextFormat("%zu", lines[i].value), {(float)(InfoX + InfoWidth - 60), (float)y}, resFontSize, 2, WHITE);
                        y += iconSize + spacing;
                    }
                    break;
                }
            }
        }
    }

    int Utils::mainMenu()
    {
        std::vector<std::string> options = {"Play", "Settings", "Quit"};
        int selected = 0;
        int optionCount = options.size();
        Texture2D MenuBackground = RaylibWrapper::LoadTexture("GUI/assets/backgrounds/MainMenu.jpg");

        while (!RaylibWrapper::WindowShouldClose()) {
            RaylibWrapper::BeginDrawing();
            RaylibWrapper::ClearBackground(RAYWHITE);
            RaylibWrapper::DrawTexture(MenuBackground, 0, 0, WHITE);

            int mainFontSize = 80;
            int subFontSize = 28;

            Vector2 mainPos = {1920/2.0f - MeasureTextEx(_cocFont, "Zappy", mainFontSize, 2).x/2, 180};
            RaylibWrapper::DrawTextEx(_cocFont, "Zappy", mainPos, mainFontSize, 2, BLACK);

            Vector2 subPos = {1920/2.0f - MeasureTextEx(_cocFont, "Clash of Clans Zappy Edition", subFontSize, 2).x/2, (float)(180 + mainFontSize + 10)};
            RaylibWrapper::DrawTextEx(_cocFont, "Clash of Clans Zappy Edition", subPos, subFontSize, 2, DARKGRAY);

            for (int i = 0; i < optionCount; ++i) {
                Color color = (i == selected) ? RED : BLACK;
                Vector2 optPos = {1920/2.0f - MeasureTextEx(_cocFont, options[i].c_str(), 30, 2).x/2, (float)(400 + i * 60)};
                RaylibWrapper::DrawTextEx(_cocFont, options[i].c_str(), optPos, 30, 2, color);
            }

            RaylibWrapper::EndDrawing();

            if (RaylibWrapper::IsKeyPressed(KEY_DOWN)) selected = (selected + 1) % optionCount;
            if (RaylibWrapper::IsKeyPressed(KEY_UP)) selected = (selected - 1 + optionCount) % optionCount;
            if (RaylibWrapper::IsKeyPressed(KEY_ENTER) || RaylibWrapper::IsKeyPressed(KEY_KP_ENTER)) break;
        }

        return selected;
    }

    void Utils::DrawTeamPanel(const Player &players)
    {
        auto counts = players.getTeamLevelCounts();
        int panelWidth = 520;
        int panelX = 40;
        int panelY = 450;
        int rowHeight = 36;
        int titleHeight = 54;
        int padding = 18;
        int teamCount = counts.size();
        int panelHeight = titleHeight + teamCount * rowHeight + padding * 2;

        RaylibWrapper::DrawRectangle(panelX, panelY, panelWidth, panelHeight, Fade(DARKGRAY, 0.8f));
        RaylibWrapper::DrawRectangleLines(panelX, panelY, panelWidth, panelHeight, BLACK);

        RaylibWrapper::DrawTextEx(_cocFont, "Levels", {(float)(panelX + 24), (float)(panelY + 18)}, 32, 2, WHITE);

        int y = panelY + titleHeight + 30;
        int teamFontSize = 22;
        int levelFontSize = 20;
        int levelStartX = panelX + 170;
        int levelSpacing = 38;

        for (int lvl = 0; lvl < 8; ++lvl) {
            RaylibWrapper::DrawTextEx(_cocFont, TextFormat("L%d", lvl + 1), {(float)(levelStartX + lvl * levelSpacing), (float)(y - 28)}, 18, 1.5f, Fade(GOLD, 0.85f));
        }

        int i = 0;
        for (const auto &team : counts) {
            Color teamColor = ColorFromHSV((float)(i * 360.0f / teamCount), 0.6f, 0.95f);
            RaylibWrapper::DrawRectangle(panelX + 10, y + 4, panelWidth - 20, rowHeight - 8, Fade(teamColor, 0.13f));
            RaylibWrapper::DrawTextEx(_cocFont, team.first.c_str(), {(float)(panelX + 24), (float)y}, teamFontSize, 2, teamColor);

            for (int lvl = 0; lvl < 8; ++lvl) {
                Color lvlColor = (team.second[lvl] > 0) ? WHITE : Fade(WHITE, 0.4f);
                RaylibWrapper::DrawTextEx(_cocFont, TextFormat("%d", team.second[lvl]), {(float)(levelStartX + lvl * levelSpacing), (float)y}, levelFontSize, 1.5f, lvlColor);
            }
            y += rowHeight;
            ++i;
        }
    }
}
