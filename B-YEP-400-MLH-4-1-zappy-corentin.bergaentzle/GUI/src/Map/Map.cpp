/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Map
*/

#include "Map.hpp"
#include <iostream>

Map::Map(int sizeX, int sizeY)
{
    tileData temp;
    temp.nbPlayers = 0;
    temp.nbEggs = 0;
    temp.nbFood = 0;
    temp.nbLinemate = 0;
    temp.nbDeraumere = 0;
    temp.nbSibur = 0;
    temp.nbMendiane = 0;
    temp.nbPhiras = 0;
    temp.nbThystame = 0;
    _nbTilesX = sizeX;
    _nbTilesY = sizeY;
    for (int x = 0; x < _nbTilesX; x++) {
        for (int y = 0; y < _nbTilesY; y++) {
            temp.coords = std::make_pair(x, y);
            temp.tilePosition = {(float)x, 0.0f, (float)y};
            _tiles.push_back(temp);
        }
    }
    _goldCoin = RaylibWrapper::LoadModel("GUI/assets/Ressources/GoldCoin.obj");
    _starryOre = RaylibWrapper::LoadModel("GUI/assets/Ressources/StarryOre.obj");
    _gem = RaylibWrapper::LoadModel("GUI/assets/Ressources/Gem.obj");
    _pinkElixir = RaylibWrapper::LoadModel("GUI/assets/Ressources/PinkElixirDrop.obj");
    _darkElixir = RaylibWrapper::LoadModel("GUI/assets/Ressources/DarkElixirDrop.obj");
    _shinyOre = RaylibWrapper::LoadModel("GUI/assets/Ressources/ShinyOre.obj");
    _food = RaylibWrapper::LoadModel("GUI/assets/Ressources/Food.obj");
    player = RaylibWrapper::LoadModel("GUI/assets/Ressources/Player.obj");
    egg = RaylibWrapper::LoadModel("GUI/assets/Ressources/Egg.obj");
    _grass = RaylibWrapper::LoadModel("GUI/assets/Ressources/Grass.obj");
    _lightGrass = RaylibWrapper::LoadModel("GUI/assets/Ressources/LightGrass.obj");
}

Map::~Map()
{
}

void Map::DrawPlayers(tileData tile) const
{
    Vector3 playerPos = {0.0f + (float)tile.coords.first, 0.1f, 0.0f + (float)tile.coords.second};
    for (std::size_t i = 0; i < tile.nbPlayers; i++) {
        RaylibWrapper::DrawModel(player, playerPos, 0.2f, WHITE);
        playerPos.y += 0.43f;
    }
}

void Map::DrawEggs(tileData tile) const
{
    Vector3 eggPos = {0.20f + (float)tile.coords.first, 0.1f, -0.20f + (float)tile.coords.second};
    for (std::size_t i = 0; i < tile.nbEggs; i++) {
        std::cout << tile.nbEggs << std::endl;
        RaylibWrapper::DrawModel(egg, eggPos, 0.02f, WHITE);
        eggPos.y += 0.2f;
    }
}

void Map::DrawFood(tileData tile) const
{
    Vector3 foodPos = {0.3f + (float)tile.coords.first, 0.15f, 0.0f + (float)tile.coords.second};
    for (std::size_t i = 0; i < tile.nbFood; i++) {
        RaylibWrapper::DrawModel(_food, foodPos, 0.05f, WHITE);
        foodPos.y += 0.13f;
    }
}

void Map::DrawLinemate(tileData tile) const
{
    Vector3 linematePos = {0.20f + (float)tile.coords.first, 0.18f, 0.20f + (float)tile.coords.second};
    for (std::size_t i = 0; i < tile.nbLinemate; i++) {
        RaylibWrapper::DrawModel(_goldCoin, linematePos, 0.07f, WHITE);
        linematePos.y += 0.13f;
    }
}

void Map::DrawDeraumere(tileData tile) const
{
    Vector3 deraumerePos = {0.0f + (float)tile.coords.first, 0.20f, 0.3f + (float)tile.coords.second};
    for (std::size_t i = 0; i < tile.nbDeraumere; i++) {
        DrawModel(_starryOre, deraumerePos, 0.05f, WHITE);
        deraumerePos.y += 0.14f;
    }
}

void Map::DrawSibur(tileData tile) const
{
    Vector3 siburPos = {-0.20f + (float)tile.coords.first, 0.19f, 0.20f + (float)tile.coords.second};
    for (std::size_t i = 0; i < tile.nbSibur; i++) {
        RaylibWrapper::DrawModel(_gem, siburPos, 0.07f, WHITE);
        siburPos.y += 0.17f;
    }
}

void Map::DrawMendiane(tileData tile) const
{
    Vector3 mendianePos = {-0.3f + (float)tile.coords.first, 0.15f, 0.0f + (float)tile.coords.second};
    for (std::size_t i = 0; i < tile.nbMendiane; i++) {
        RaylibWrapper::DrawModel(_pinkElixir, mendianePos, 0.05f, WHITE);
        mendianePos.y += 0.17f;
    }
}

void Map::DrawPhiras(tileData tile) const
{
    Vector3 phirasPos = {-0.20f + (float)tile.coords.first, 0.15f, -0.20f + (float)tile.coords.second};
    for (std::size_t i = 0; i < tile.nbPhiras; i++) {
        RaylibWrapper::DrawModel(_darkElixir, phirasPos, 0.05f, WHITE);
        phirasPos.y += 0.17f;
    }
}

void Map::DrawThystame(tileData tile) const
{
    Vector3 thystamePos = {0.0f + (float)tile.coords.first, 0.20f, -0.3f + (float)tile.coords.second};
    for (std::size_t i = 0; i < tile.nbThystame; i++) {
        RaylibWrapper::DrawModel(_shinyOre, thystamePos, 0.04f, WHITE);
        thystamePos.y += 0.12f;
    }
}

void Map::DrawTileElements(tileData tile) const
{
    if (tile.nbPlayers > 0) {
        DrawPlayers(tile);
    }
    if (tile.nbEggs > 0) {
        DrawEggs(tile);
    }
    if (tile.nbFood > 0) {
        DrawFood(tile);
    }
    if (tile.nbLinemate > 0) {
        DrawLinemate(tile);
    }
    if (tile.nbDeraumere > 0) {
        DrawDeraumere(tile);
    }
    if (tile.nbSibur > 0) {
        DrawSibur(tile);
    }
    if (tile.nbMendiane > 0) {
        DrawMendiane(tile);
    }
    if (tile.nbPhiras > 0) {
        DrawPhiras(tile);
    }
    if (tile.nbThystame > 0) {
        DrawThystame(tile);
    }
}

std::vector<Map::tileData> Map::getTiles() const
{
    return _tiles;
}

Vector3 Map::getTilePositionFrom(int x, int y) const
{
    for (auto i : _tiles) {
        if (i.coords.first == x && i.coords.second == y){
            return i.tilePosition;
        }
    }
    return {0.0f, 0.0f, 0.0f};
}

std::size_t Map::getPlayersOnTileFrom(int x, int y) const
{
    for (auto i : _tiles) {
        if (i.coords.first == x && i.coords.second == y){
            return i.nbPlayers;
        }
    }
    return 0;
}

std::size_t Map::getEggsOnTileFrom(int x, int y) const
{
    for (auto i : _tiles) {
        if (i.coords.first == x && i.coords.second == y){
            return i.nbEggs;
        }
    }
    return 0;
}

std::size_t Map::getFoodsOnTileFrom(int x, int y) const
{
    for (auto i : _tiles) {
        if (i.coords.first == x && i.coords.second == y){
            return i.nbFood;
        }
    }
    return 0;
}

std::size_t Map::getFoodsOnMap() const
{
    std::size_t totalFood = 0;
    for (std::size_t x = 0; x < _tiles.size(); x++) {
        totalFood += getFoodsOnTileFrom(_tiles[x].coords.first, _tiles[x].coords.second);
    }
    return totalFood;
}

std::size_t Map::getLinematesOnTileFrom(int x, int y) const
{
    for (auto i : _tiles) {
        if (i.coords.first == x && i.coords.second == y){
            return i.nbLinemate;
        }
    }
    return 0;
}

std::size_t Map::getLinematesOnMap() const
{
    std::size_t totalLinemates = 0;
    for (std::size_t x = 0; x < _tiles.size(); x++) {
        totalLinemates += getLinematesOnTileFrom(_tiles[x].coords.first, _tiles[x].coords.second);
    }
    return totalLinemates;
}

std::size_t Map::getDeraumeresOnTileFrom(int x, int y) const
{
    for (auto i : _tiles) {
        if (i.coords.first == x && i.coords.second == y){
            return i.nbDeraumere;
        }
    }
    return 0;
}

std::size_t Map::getDeraumeresOnMap() const
{
    std::size_t totalDeraumeres = 0;
    for (std::size_t x = 0; x < _tiles.size(); x++) {
        totalDeraumeres += getDeraumeresOnTileFrom(_tiles[x].coords.first, _tiles[x].coords.second);
    }
    return totalDeraumeres;
}

std::size_t Map::getSibursOnTileFrom(int x, int y) const
{
    for (auto i : _tiles) {
        if (i.coords.first == x && i.coords.second == y){
            return i.nbSibur;
        }
    }
    return 0;
}

std::size_t Map::getSibursOnMap() const
{
    std::size_t totalSiburs = 0;
    for (std::size_t x = 0; x < _tiles.size(); x++) {
        totalSiburs += getSibursOnTileFrom(_tiles[x].coords.first, _tiles[x].coords.second);
    }
    return totalSiburs;
}

std::size_t Map::getMendianesOnTileFrom(int x, int y) const
{
    for (auto i : _tiles) {
        if (i.coords.first == x && i.coords.second == y){
            return i.nbMendiane;
        }
    }
    return 0;
}

std::size_t Map::getMendianesOnMap() const
{
    std::size_t totalMendianes = 0;
    for (std::size_t x = 0; x < _tiles.size(); x++) {
        totalMendianes += getMendianesOnTileFrom(_tiles[x].coords.first, _tiles[x].coords.second);
    }
    return totalMendianes;
}

std::size_t Map::getPhirasOnTileFrom(int x, int y) const
{
    for (auto i : _tiles) {
        if (i.coords.first == x && i.coords.second == y){
            return i.nbPhiras;
        }
    }
    return 0;
}

std::size_t Map::getPhirasOnMap() const
{
    std::size_t totalPhiras = 0;
    for (std::size_t x = 0; x < _tiles.size(); x++) {
        totalPhiras += getPhirasOnTileFrom(_tiles[x].coords.first, _tiles[x].coords.second);
    }
    return totalPhiras;
}

std::size_t Map::getThystamesOnTileFrom(int x, int y) const
{
    for (auto i : _tiles) {
        if (i.coords.first == x && i.coords.second == y){
            return i.nbThystame;
        }
    }
    return 0;
}

std::size_t Map::getThystamesOnMap() const
{
    std::size_t totalThystames = 0;
    for (std::size_t x = 0; x < _tiles.size(); x++) {
        totalThystames += getThystamesOnTileFrom(_tiles[x].coords.first, _tiles[x].coords.second);
    }
    return totalThystames;
}

void Map::setGameTime(int time)
{
    _time = time;
}

void Map::setNbPlayers(std::size_t nb, int x, int y)
{
    for (auto &i : _tiles) {
        if (i.coords.first == x && i.coords.second == y){
            i.nbPlayers = nb;
            return;
        }
    }
    return;
}

void Map::setNbEggs(std::size_t nb, int x, int y)
{
    for (auto &i : _tiles) {
        if (i.coords.first == x && i.coords.second == y){
            i.nbEggs = nb;
            return;
        }
    }
    return;
}

void Map::setNbFoods(std::size_t nb, int x, int y)
{
    for (auto &i : _tiles) {
        if (i.coords.first == x && i.coords.second == y){
            i.nbFood = nb;
            return;
        }
    }
    return;
}

void Map::setNbLinemates(std::size_t nb, int x, int y)
{
    for (auto &i : _tiles) {
        if (i.coords.first == x && i.coords.second == y){
            i.nbLinemate = nb;
            return;
        }
    }
    return;
}

void Map::setNbDeraumeres(std::size_t nb, int x, int y)
{
    for (auto &i : _tiles) {
        if (i.coords.first == x && i.coords.second == y){
            i.nbDeraumere = nb;
            return;
        }
    }
    return;
}

void Map::setNbSiburs(std::size_t nb, int x, int y)
{
    for (auto &i : _tiles) {
        if (i.coords.first == x && i.coords.second == y){
            i.nbSibur = nb;
            return;
        }
    }
    return;
}

void Map::setNbMendianes(std::size_t nb, int x, int y)
{
    for (auto &i : _tiles) {
        if (i.coords.first == x && i.coords.second == y){
            i.nbMendiane = nb;
            return;
        }
    }
    return;
}

void Map::setNbPhiras(std::size_t nb, int x, int y)
{
    for (auto &i : _tiles) {
        if (i.coords.first == x && i.coords.second == y){
            i.nbPhiras = nb;
            return;
        }
    }
    return;
}

void Map::setNbThystames(std::size_t nb, int x, int y)
{
    for (auto &i : _tiles) {
        if (i.coords.first == x && i.coords.second == y){
            i.nbThystame = nb;
            return;
        }
    }
    return;
}

void Map::setSize(int sizeX, int sizeY)
{
    _nbTilesX = sizeX;
    _nbTilesY = sizeY;
    _tiles.clear();
    tileData temp;
    temp.nbPlayers = 0;
    temp.nbFood = 0;
    temp.nbLinemate = 0;
    temp.nbDeraumere = 0;
    temp.nbSibur = 0;
    temp.nbMendiane = 0;
    temp.nbPhiras = 0;
    temp.nbThystame = 0;
    temp.nbEggs = 0;
    for (int x = 0; x < _nbTilesX; x++) {
        for (int y = 0; y < _nbTilesY; y++) {
            temp.coords = std::make_pair(x, y);
            temp.tilePosition = {(float)x, 0.0f, (float)y};
            _tiles.push_back(temp);
        }
    }
}

void Map::DrawMap() const
{
    Color grassColor = Color{80, 200, 120, 255};
    for (auto i : _tiles) {
        bool isLight = (i.coords.first + i.coords.second) % 2 == 0;
        RaylibWrapper::DrawModel(isLight ? _grass : _lightGrass, i.tilePosition, 0.5f, grassColor);
        DrawTileElements(i);
    }
}