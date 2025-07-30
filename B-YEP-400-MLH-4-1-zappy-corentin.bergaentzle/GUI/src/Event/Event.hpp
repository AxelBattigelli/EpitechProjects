/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Event
*/

#pragma once
    #include "../Wrappers/Raylib/RaylibWrapper.hpp"
    #include "../Particles/Particles.hpp"
    #include "../CameraController/CameraController.hpp"


class Event {
    public:
        Event(CameraController &cam, ParticlesSystem &particles);
        ~Event();
        void HandleEvent();

    private:
    CameraController &cam;
    ParticlesSystem &particles;
};
