/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Player
*/

#pragma once

#include <array>
#include <map>
#include <string>
#include <unordered_map>
#include <vector>
#include "../Map/Map.hpp"
#include "../ShockWaves/ShockWaves.hpp"

enum PlayerOrientation {
    NORTH = 1,
    EAST = 2,
    SOUTH = 3,
    WEST = 4
};

struct PlayerData {
    int id;
    int x, y;
    PlayerOrientation orientation;
    int level;
    std::string team;
    std::vector<int> inventory;
};

class Player {
public:
    Player();
    ~Player();
    void addPlayer(int id, int x, int y, std::string orientation, int level, const std::string &team);
    void updatePosition(int id, int x, int y, std::string orientation);
    void updateLevel(int id, int level);
    void updateInventory(int id, int x, int y, const std::vector<int> &inventory);
    void handleExpulsion(int id);
    void handleDrop(int id);
    void handlePickup(int id);
    void removePlayer(int id);
    void drawPlayersOnMap(const Map &map) const;
    void addBroadCast(int x, int y, Color color);
    void UpdateBroadCasts(float delta);
    void EraseEmptyBroadCasts();
    void DrawBroadCasts();
    std::map<std::string, std::vector<std::pair<int, std::vector<int>>>> getTeamInventories() const;

    const std::unordered_map<int, PlayerData>& getPlayers() const { return players; }
    std::map<std::string, std::array<int, 8>> getTeamLevelCounts() const;
    int getPlayerlvlbyid(int id) const;
    std::string getTeam(int playerId) const;
    std::pair<int, int> getPlayerPosition(int id) const;
    std::unordered_map<int, PlayerData> players;

private:
    std::vector<ShockWaves> _broadcasts;
};

