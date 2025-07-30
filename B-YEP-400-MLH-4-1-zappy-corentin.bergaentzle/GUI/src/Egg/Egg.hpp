/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Egg
*/

#pragma once

#include <string>
#include <unordered_map>
#include "../Map/Map.hpp"

struct EggData {
    int id;
    int x, y;
    std::string team;
};

class Egg {
public:
    Egg();
    ~Egg();
    void addEgg(int id, int x, int y, const std::string &team);
    void removeEgg(int id);
    void drawEggsOnMap(const Map &map) const;
    std::pair<int, int> getEggPosition(int id) const;
    std::string getEggTeam(int id) const;

    const std::unordered_map<int, EggData>& getEggs() const { return eggs; }

private:
    std::unordered_map<int, EggData> eggs;
};

