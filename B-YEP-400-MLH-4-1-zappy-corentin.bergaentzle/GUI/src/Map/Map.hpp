/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Map
*/

#pragma once

    #include <utility>
    #include <vector>
    #include "../Wrappers/Raylib/RaylibWrapper.hpp"

class Map {
    public:
        struct tileData{
            std::pair<int, int> coords;
            Vector3 tilePosition;
            std::size_t nbPlayers;
            std::size_t nbEggs;
            std::size_t nbFood;
            std::size_t nbLinemate;
            std::size_t nbDeraumere;
            std::size_t nbSibur;
            std::size_t nbMendiane;
            std::size_t nbPhiras;
            std::size_t nbThystame;
        };
        Map(int sizeX, int sizeY);
        ~Map();
        void setSize(int sizeX, int sizeY);
        void DrawMap() const;
        void DrawTileElements(tileData tile) const;
        void DrawPlayers(tileData tile) const;
        void DrawEggs(tileData tile) const;
        void DrawFood(tileData tile) const;
        void DrawLinemate(tileData tile) const;
        void DrawDeraumere(tileData tile) const;
        void DrawSibur(tileData tile) const;
        void DrawMendiane(tileData tile) const;
        void DrawPhiras(tileData tile) const;
        void DrawThystame(tileData tile) const;
        std::vector<tileData> getTiles() const;
        Vector3 getTilePositionFrom(int x, int y) const;
        std::size_t getPlayersOnTileFrom(int x, int y)const ;
        std::size_t getEggsOnTileFrom(int x, int y)const ;
        std::size_t getFoodsOnTileFrom(int x, int y)const ;
        std::size_t getLinematesOnTileFrom(int x, int y)const ;
        std::size_t getDeraumeresOnTileFrom(int x, int y)const ;
        std::size_t getSibursOnTileFrom(int x, int y)const ;
        std::size_t getMendianesOnTileFrom(int x, int y)const ;
        std::size_t getPhirasOnTileFrom(int x, int y)const ;
        std::size_t getThystamesOnTileFrom(int x, int y)const ;
        void setNbPlayers(std::size_t nb, int x, int y);
        void setNbEggs(std::size_t nb, int x, int y);
        void setNbFoods(std::size_t nb, int x, int y);
        void setNbLinemates(std::size_t nb, int x, int y);
        void setNbDeraumeres(std::size_t nb, int x, int y);
        void setNbSiburs(std::size_t nb, int x, int y);
        void setNbMendianes(std::size_t nb, int x, int y);
        void setNbPhiras(std::size_t nb, int x, int y);
        void setNbThystames(std::size_t nb, int x, int y);
        size_t getFoodsOnMap() const;
        std::size_t getLinematesOnMap() const;
        std::size_t getDeraumeresOnMap() const;
        std::size_t getSibursOnMap() const;
        std::size_t getMendianesOnMap() const;
        std::size_t getPhirasOnMap() const;
        std::size_t getThystamesOnMap() const;
        int _time;
        void setGameTime(int time);
        Model player;
        Model egg;

        const std::vector<int> &getTileResources(int x, int y) const;
    private:
        int _nbTilesX;
        int _nbTilesY;
        std::vector<tileData> _tiles;
        Model _goldCoin;
        Model _starryOre;
        Model _gem;
        Model _pinkElixir;
        Model _darkElixir;
        Model _shinyOre;
        Model _food;
        Model _grass;
        Model _lightGrass;
};
