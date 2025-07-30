/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Egg
*/

#include "Egg.hpp"
#include "../Map/Map.hpp"
#include "../Wrappers/Raylib/RaylibWrapper.hpp"
#include <unordered_map>

Egg::Egg()
{}

Egg::~Egg()
{}

void Egg::addEgg(int id, int x, int y, const std::string &team)
{
    EggData p{id, x, y, team};
    eggs[id] = p;
}

void Egg::removeEgg(int id)
{
    eggs.erase(id);
}

void Egg::drawEggsOnMap(const Map &map) const
{
    float angle = 0.0f;
    Vector3 scale = {0.04f, 0.04f, 0.04f};
    Vector3 axis = {0.0f, 0.1f, 0.0f};

    for (const auto &pair : eggs) {
        const EggData &p = pair.second;
        Vector3 pos = map.getTilePositionFrom(p.x, p.y);
        pos.y += 0.25f;
        RaylibWrapper::DrawModelEx(map.egg, pos, axis, angle, scale, WHITE);
    }
}

std::pair<int, int> Egg::getEggPosition(int id) const
{
    if (eggs.count(id)) {
        return {eggs.at(id).x, eggs.at(id).y};
    }
    return {-1, -1};
}

std::string Egg::getEggTeam(int id) const
{
    if (eggs.count(id)) {
        return eggs.at(id).team;
    }
    return "";
}