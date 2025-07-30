/*
** EPITECH PROJECT, 2025
** ZappyKomJamet
** File description:
** ProtocolHandler
*/

#include "ProtocolHandler.hpp"
#include <sstream>
#include <iostream>

#define COLOR_DEBUG    "\033[1;35m"
#define COLOR_TAG      "\033[1;36m"
#define COLOR_MSG      "\033[0;37m"
#define COLOR_RESET    "\033[0m"

void ProtocolHandler::debugPrint(const std::string &tag, const std::string &msg)
{
    std::cout << COLOR_DEBUG << "[DEBUG-RECEIVE] " << COLOR_TAG << tag << " " << COLOR_MSG << msg << COLOR_RESET << std::endl;
}

void ProtocolHandler::handleMessage(const std::string& message, bool debugMode)
{
    std::istringstream iss(message);
    std::vector<std::string> tokens;
    std::string token;
    while (iss >> token)
        tokens.push_back(token);

    if (tokens.empty())
        return;

    if (tokens[0] == "msz") handleMsz(tokens, debugMode);
    else if (tokens[0] == "bct") handleBct(tokens, debugMode);
    else if (tokens[0] == "mct") handleMct(tokens, debugMode);
    else if (tokens[0] == "tna") handleTna(tokens, debugMode);
    else if (tokens[0] == "pnw") handlePnw(tokens, debugMode);
    else if (tokens[0] == "ppo") handlePpo(tokens, debugMode);
    else if (tokens[0] == "plv") handlePlv(tokens, debugMode);
    else if (tokens[0] == "pin") handlePin(tokens, debugMode);
    else if (tokens[0] == "pex") handlePex(tokens, debugMode);
    else if (tokens[0] == "pbc") handlePbc(tokens, debugMode);
    else if (tokens[0] == "pic") handlePic(tokens, debugMode);
    else if (tokens[0] == "pie") handlePie(tokens, debugMode);
    else if (tokens[0] == "pfk") handlePfk(tokens, debugMode);
    else if (tokens[0] == "pdr") handlePdr(tokens, debugMode);
    else if (tokens[0] == "pgt") handlePgt(tokens, debugMode);
    else if (tokens[0] == "pdi") handlePdi(tokens, debugMode);
    else if (tokens[0] == "enw") handleEnw(tokens, debugMode);
    else if (tokens[0] == "ebo") handleEbo(tokens, debugMode);
    else if (tokens[0] == "edi") handleEdi(tokens, debugMode);
    else if (tokens[0] == "sgt") handleSgt(tokens, debugMode);
    else if (tokens[0] == "sst") handleSst(tokens, debugMode);
    else if (tokens[0] == "seg") handleSeg(tokens, debugMode);
    else if (tokens[0] == "smg") handleSmg(tokens, debugMode);
    else if (tokens[0] == "suc") handleSuc(tokens, debugMode);
    else if (tokens[0] == "sbp") handleSbp(tokens, debugMode);
    else std::cerr << "Unknown message: " << message << std::endl;
}

void ProtocolHandler::handleMsz(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() != 3)
        return;
    int width = std::stoi(tokens[1]);
    int height = std::stoi(tokens[2]);
    map.setSize(width, height);
    if (debugMode)
        debugPrint("[MSZ]", "Map size set to: " + std::to_string(width) + "x" + std::to_string(height));
}

void ProtocolHandler::handleBct(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() < 10)
        return;
    int x = std::stoi(tokens[1]);
    int y = std::stoi(tokens[2]);
    map.setNbFoods(std::stoi(tokens[3]), x, y);
    map.setNbLinemates(std::stoi(tokens[4]), x, y);
    map.setNbDeraumeres(std::stoi(tokens[5]), x, y);
    map.setNbSiburs(std::stoi(tokens[6]), x, y);
    map.setNbMendianes(std::stoi(tokens[7]), x, y);
    map.setNbPhiras(std::stoi(tokens[8]), x, y);
    map.setNbThystames(std::stoi(tokens[9]), x, y);
    if (debugMode) {
        debugPrint("[BCT]", "Tile (" + std::to_string(x) + "," + std::to_string(y) + ") resources: " +
            tokens[3] + " " + tokens[4] + " " + tokens[5] + " " +
            tokens[6] + " " + tokens[7] + " " + tokens[8] + " " + tokens[9]);
    }
}

void ProtocolHandler::handleMct(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() < 10 || tokens.size() % 8 != 2)
        return;

    for (size_t i = 1; i < tokens.size(); i += 8) {
        int x = std::stoi(tokens[i]);
        int y = std::stoi(tokens[i + 1]);
        map.setNbFoods(std::stoi(tokens[i + 2]), x, y);
        map.setNbLinemates(std::stoi(tokens[i + 3]), x, y);
        map.setNbDeraumeres(std::stoi(tokens[i + 4]), x, y);
        map.setNbSiburs(std::stoi(tokens[i + 5]), x, y);
        map.setNbMendianes(std::stoi(tokens[i + 6]), x, y);
        map.setNbPhiras(std::stoi(tokens[i + 7]), x, y);
        if (debugMode) {
            debugPrint("[BCT]", "Tile (" + std::to_string(x) + "," + std::to_string(y) + ") resources: " +
                tokens[i + 2] + " " + tokens[i + 3] + " " + tokens[i + 4] + " " +
                tokens[i + 5] + " " + tokens[i + 6] + " " + tokens[i + 7]);
        }
    }
}

void ProtocolHandler::handleTna(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() != 2)
        return;
    std::string teamName = tokens[1];
    if (debugMode)
        debugPrint("[TNA]", "Team name: " + teamName);
}

void ProtocolHandler::handlePnw(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() != 7)
        return;
    int id = std::stoi(tokens[1].substr(1));
    int x = std::stoi(tokens[2]);
    int y = std::stoi(tokens[3]);
    std::string orientation = tokens[4];
    int level = std::stoi(tokens[5]);
    std::string team = tokens[6];
    if (debugMode)
        debugPrint("[PNW]", "Player #" + std::to_string(id) + " at (" + std::to_string(x) + "," + std::to_string(y) +
            ") orientation " + orientation + " level " + std::to_string(level) + " team " + team);
    players.addPlayer(id, x, y, orientation, level, team);
}

void ProtocolHandler::handlePpo(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() != 5)
        return;
    int id = std::stoi(tokens[1].substr(1));
    int x = std::stoi(tokens[2]);
    int y = std::stoi(tokens[3]);
    std::string orientation = tokens[4];
    if (debugMode)
        debugPrint("[PPO]", "Player #" + std::to_string(id) + " moved to (" + std::to_string(x) + "," + std::to_string(y) +
            ") orientation " + orientation);
    players.updatePosition(id, x, y, orientation);
}

void ProtocolHandler::handlePlv(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() != 3)
        return;
    int id = std::stoi(tokens[1].substr(1));
    int level = std::stoi(tokens[2]);
    if (debugMode)
        debugPrint("[PLV]", "Player #" + std::to_string(id) + " level updated to " + std::to_string(level));
    players.updateLevel(id, level);
}

void ProtocolHandler::handlePin(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() != 11)
        return;
    int id = std::stoi(tokens[1].substr(1));
    int x = std::stoi(tokens[2]);
    int y = std::stoi(tokens[3]);
    std::vector<int> inventory;
    for (int i = 4; i <= 10; ++i)
        inventory.push_back(std::stoi(tokens[i]));
    if (debugMode) {
        std::string msg = "Player #" + std::to_string(id) + " inventory at (" + std::to_string(x) + "," + std::to_string(y) + "): ";
        for (const auto &res : inventory)
            msg += std::to_string(res) + " ";
        debugPrint("[PIN]", msg);
    }
    players.updateInventory(id, x, y, inventory);
}

void ProtocolHandler::handlePex(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() != 2)
        return;
    int id = std::stoi(tokens[1].substr(1));
    if (debugMode)
        debugPrint("[PEX]", "Player #" + std::to_string(id) + " has expelled others from their tile.");
    players.handleExpulsion(id);
}

void ProtocolHandler::handlePbc(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() < 3)
        return;
    int id = std::stoi(tokens[1].substr(1));
    std::string message = tokens[2];
    const std::unordered_map<int, PlayerData> tempPlayer = players.getPlayers();
    if (tempPlayer.find(id) != tempPlayer.end()) {
        players.addBroadCast(tempPlayer.at(id).x, tempPlayer.at(id).y, RED);
    }
    if (debugMode)
        debugPrint("[PBC]", "Player #" + std::to_string(id) + " broadcasted message: " + message);
}

void ProtocolHandler::handlePic(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() < 5)
        return;
    int x = std::stoi(tokens[1]);
    int y = std::stoi(tokens[2]);
    Vector3 pos = map.getTilePositionFrom(x, y);
    players.addBroadCast(pos.x, pos.y, GREEN);
    if (debugMode)
        debugPrint("[PIC]", "Incantation at (" + tokens[1] + "," + tokens[2] + ") level " + tokens[3]);
}

void ProtocolHandler::handlePie(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() != 4)
        return;
    int x = std::stoi(tokens[1]);
    int y = std::stoi(tokens[2]);
    auto pos = map.getTilePositionFrom(x, y);
    particles.SpawnParticles(pos);
    if (debugMode)
        debugPrint("[PIE]", "Incantation at (" + std::to_string(x) + "," + std::to_string(y) + ") ended.");
}

void ProtocolHandler::handlePfk(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() != 2)
        return;
    if (debugMode)
        debugPrint("[PFK]", "Player #" + tokens[1] + " has failed an incantation.");
}

void ProtocolHandler::handlePdr(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() != 2)
        return;
    int id = std::stoi(tokens[1].substr(1));
    players.handleDrop(id);
    if (debugMode)
        debugPrint("[PDR]", "Player #" + std::to_string(id) + " dropped a resource.");
}

void ProtocolHandler::handlePgt(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() != 2)
        return;
    int id = std::stoi(tokens[1].substr(1));
    players.handlePickup(id);
    if (debugMode)
        debugPrint("[PGT]", "Player #" + std::to_string(id) + " picked up a resource.");
}

void ProtocolHandler::handlePdi(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() != 2)
        return;
    int id = std::stoi(tokens[1].substr(1));
    if (debugMode)
        debugPrint("[PDI]", "Player #" + std::to_string(id) + " died.");
    players.removePlayer(id);
}

void ProtocolHandler::handleEnw(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() != 5)
        return;
    int eggId = std::stoi(tokens[1].substr(1));
    int playerId = std::stoi(tokens[2].substr(1));
    int x = std::stoi(tokens[3]);
    int y = std::stoi(tokens[4]);
    std::string team = players.getTeam(playerId);
    eggs.addEgg(eggId, x, y, team);
    if (debugMode)
        debugPrint("[ENW]", "Player #" + std::to_string(playerId) + " has spawned an egg with ID " + std::to_string(eggId) +
            " at (" + std::to_string(x) + "," + std::to_string(y) + ")");
}

void ProtocolHandler::handleEbo(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() != 2)
        return;
    int eggId = std::stoi(tokens[1].substr(1));
    eggs.removeEgg(eggId);
    if (debugMode)
        debugPrint("[EBO]", "Egg with ID " + std::to_string(eggId) + " has been laid.");
    // Ajoute ici le code pour gérer l'œuf si besoin
}

void ProtocolHandler::handleEdi(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() != 2)
        return;
    int eggId = std::stoi(tokens[1].substr(1));
    eggs.removeEgg(eggId);
    if (debugMode)
        debugPrint("[EDI]", "Egg with ID " + std::to_string(eggId) + " has been hatched.");
    // Ajoute ici le code pour gérer l'œuf si besoin
}

void ProtocolHandler::handleSgt(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() != 2)
        return;
    int time = std::stoi(tokens[1]);
    if (debugMode)
        debugPrint("[SGT]", "Game time set to " + std::to_string(time) + " seconds.");
    map.setGameTime(time);
}

void ProtocolHandler::handleSst(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() != 3)
        return;
    int time = std::stoi(tokens[1]);
    if (debugMode)
        debugPrint("[SST]", "Special tile at (" + tokens[1] + "," + tokens[2] + ") activated.");
    map.setGameTime(time);
}

void ProtocolHandler::handleSeg(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() != 2)
        return;
    std::string team = tokens[1];
    if (debugMode)
        debugPrint("[SEG]", "Team " + team + " has won the game!");
}

void ProtocolHandler::handleSmg(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() < 2)
        return;
    std::string message = tokens[1];
    for (size_t i = 2; i < tokens.size(); ++i) {
        message += " " + tokens[i];
    }
    if (debugMode)
        debugPrint("[SMG]", "Server message: " + message);
}

void ProtocolHandler::handleSuc(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() != 1)
        return;
    if (debugMode)
        debugPrint("[SUC]", "Command succeeded.");
}

void ProtocolHandler::handleSbp(const std::vector<std::string> &tokens, bool debugMode)
{
    if (tokens.size() != 1)
        return;
    if (debugMode)
        debugPrint("[SBP]", "Command failed.");
}
