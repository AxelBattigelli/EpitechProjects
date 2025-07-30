/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Camera
*/

#pragma once

    #include "../Wrappers/Raylib/RaylibWrapper.hpp"

class CameraController {
    public:
        CameraController();
        void Update();
        Camera GetCamera() const;
        void SetCamera(const Camera &cam);
    private:
        Camera camera;
};

