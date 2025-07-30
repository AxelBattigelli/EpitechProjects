/*
    ** EPITECH PROJECT, 2025
    ** Zappy
    ** File description:
    ** Player
    */

    #include "Player.hpp"
    #include "../Map/Map.hpp"
    #include "../Wrappers/Raylib/RaylibWrapper.hpp"
    #include <unordered_map>
    #include <algorithm>
    #include <array>
    #include <map>

    Player::Player()
    {}

    Player::~Player()
    {}

    void Player::addPlayer(int id, int x, int y, std::string orientation, int level, const std::string &team)
    {
        PlayerOrientation cardPoint;

        if (orientation == "1")
            cardPoint = PlayerOrientation::NORTH;
        if (orientation == "2")
            cardPoint = PlayerOrientation::EAST;
        if (orientation == "3")
            cardPoint = PlayerOrientation::SOUTH;
        if (orientation == "4")
            cardPoint = PlayerOrientation::WEST;
        PlayerData p{id, x, y, cardPoint, level, team, std::vector<int>(7, 0)};
        players[id] = p;
    }

    void Player::updatePosition(int id, int x, int y, std::string orientation)
    {
        PlayerOrientation cardPoint;

        if (orientation == "1")
            cardPoint = PlayerOrientation::NORTH;
        if (orientation == "2")
            cardPoint = PlayerOrientation::EAST;
        if (orientation == "3")
            cardPoint = PlayerOrientation::SOUTH;
        if (orientation == "4")
            cardPoint = PlayerOrientation::WEST;
        if (players.count(id)) {
            players[id].x = x;
            players[id].y = y;
            players[id].orientation = cardPoint;
        }
    }

    void Player::updateLevel(int id, int level)
    {
        if (players.count(id)) {
            players[id].level = level;
        }
    }

    void Player::updateInventory(int id, int x, int y, const std::vector<int> &inventory)
    {
        if (players.count(id)) {
            players[id].x = x;
            players[id].y = y;
            players[id].inventory = inventory;
        }
    }

    void Player::handleExpulsion(int id)
    {

        if (players.count(id)) {
            players[id].x += 1;
        }
    }

    void Player::handleDrop(int id)
    {
        if (players.count(id) && !players[id].inventory.empty()) {
            if (players[id].inventory[0] > 0)
                players[id].inventory[0] -= 1;
        }
    }

    void Player::handlePickup(int id)
    {
        if (players.count(id) && !players[id].inventory.empty()) {
            players[id].inventory[0] += 1;
        }
    }

    void Player::removePlayer(int id)
    {
        players.erase(id);
    }

    void Player::drawPlayersOnMap(const Map &map) const
    {
        float angle = 0.0f;
        Vector3 scale = {0.25f, 0.25f, 0.25f};
        Vector3 axis = {0.0f, 0.1f, 0.0f};

        for (const auto &pair : players) {
            const PlayerData &p = pair.second;
            Vector3 pos = map.getTilePositionFrom(p.x, p.y);
            pos.y += 0.5f;
            if (p.orientation == PlayerOrientation::NORTH){
                angle = 0.0f;
            }
            if (p.orientation == PlayerOrientation::EAST) {
                angle = 90.0f;
            }
            if (p.orientation == PlayerOrientation::SOUTH) {
                angle = 180.0f;
            }
            if (p.orientation == PlayerOrientation::WEST) {
                angle = 270.0f;
            }
            RaylibWrapper::DrawModelEx(map.player, pos, axis, angle, scale, WHITE);
        }
    }

    std::map<std::string, std::vector<std::pair<int, std::vector<int>>>> Player::getTeamInventories() const
    {
        std::map<std::string, std::vector<std::pair<int, std::vector<int>>>> teamInventories;
        for (const auto &pair : players) {
            const PlayerData &p = pair.second;
            teamInventories[p.team].emplace_back(p.id, p.inventory);
        }
        return teamInventories;
    }

    std::map<std::string, std::array<int, 8>> Player::getTeamLevelCounts() const
    {
        std::map<std::string, std::array<int, 8>> counts;
        for (const auto &pair : players) {
            const PlayerData &p = pair.second;
            if (p.level >= 1 && p.level <= 8)
                counts[p.team][p.level - 1]++;
        }
        return counts;
    }

    std::pair<int, int> Player::getPlayerPosition(int id) const
    {
        if (players.count(id)) {
            return {players.at(id).x, players.at(id).y};
        }
        return {-1, -1};
    }
    void Player::addBroadCast(int x, int y, Color color)
    {
        Vector3 position = {(float)x, 1.15f, (float)y};
        _broadcasts.emplace_back(position, 30.0, 6.0f, color);
    }

    void Player::UpdateBroadCasts(float delta)
    {
        for (auto &b : _broadcasts) {
            b.Update(delta);
        }
    }

    void Player::EraseEmptyBroadCasts()
    {
        for (size_t i = 0; i < _broadcasts.size(); ) {
            if (!_broadcasts[i]._active)
                _broadcasts.erase(_broadcasts.begin() + i);
            else
                ++i;
        }
    }

    void Player::DrawBroadCasts()
    {
        for (auto &b : _broadcasts) {
            b.Draw();
        }
    }


    std::string Player::getTeam(int playerId) const
    {
        auto it = players.find(playerId);
        if (it != players.end())
            return it->second.team;
        return "";
    }

    int Player::getPlayerlvlbyid(int id) const
    {
        if (players.count(id)) {
            return players.at(id).level;
        }
        return 1;
    }
