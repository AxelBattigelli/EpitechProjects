/*
** EPITECH PROJECT, 2025
** ZappyKomJamet
** File description:
** ProtocolHandler
*/


#pragma once
    #include <string>
    #include <vector>
    #include "../Map/Map.hpp"
    #include "../Player/Player.hpp"
    #include "../Egg/Egg.hpp"
    #include "../Particles/Particles.hpp"

class ProtocolHandler {
    public:
        ProtocolHandler(Map &map, Player &players, Egg &eggs, ParticlesSystem &particles) : map(map), players(players), eggs(eggs) , particles(particles){}
        void handleMessage(const std::string &message, bool debugMode);
        void debugPrint(const std::string &tag, const std::string &msg);

    private:
        Map &map;
        Player &players;
        Egg &eggs;
        ParticlesSystem &particles;
        void handleMsz(const std::vector<std::string> &tokens, bool debugMode);
        void handleBct(const std::vector<std::string> &tokens, bool debugMode);
        void handleMct(const std::vector<std::string> &tokens, bool debugMode);
        void handleTna(const std::vector<std::string> &tokens, bool debugMode);
        void handlePnw(const std::vector<std::string> &tokens, bool debugMode);
        void handlePpo(const std::vector<std::string> &tokens, bool debugMode);
        void handlePlv(const std::vector<std::string> &tokens, bool debugMode);
        void handlePin(const std::vector<std::string> &tokens, bool debugMode);
        void handlePex(const std::vector<std::string> &tokens, bool debugMode);
        void handlePbc(const std::vector<std::string> &tokens, bool debugMode);
        void handlePic(const std::vector<std::string> &tokens, bool debugMode);
        void handlePie(const std::vector<std::string> &tokens, bool debugMode);
        void handlePfk(const std::vector<std::string> &tokens, bool debugMode);
        void handlePdr(const std::vector<std::string> &tokens, bool debugMode);
        void handlePgt(const std::vector<std::string> &tokens, bool debugMode);
        void handlePdi(const std::vector<std::string> &tokens, bool debugMode);
        void handleEnw(const std::vector<std::string> &tokens, bool debugMode);
        void handleEbo(const std::vector<std::string> &tokens, bool debugMode);
        void handleEdi(const std::vector<std::string> &tokens, bool debugMode);
        void handleSgt(const std::vector<std::string> &tokens, bool debugMode);
        void handleSst(const std::vector<std::string> &tokens, bool debugMode);
        void handleSeg(const std::vector<std::string> &tokens, bool debugMode);
        void handleSmg(const std::vector<std::string> &tokens, bool debugMode);
        void handleSuc(const std::vector<std::string> &tokens, bool debugMode);
        void handleSbp(const std::vector<std::string> &tokens, bool debugMode);
};