/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Camera
*/

#include "CameraController.hpp"

CameraController::CameraController()
{
    camera.position = {5.0f, 2.0f, -8.0f};
    camera.target = {5.0f, 0.0f, 5.0f};
    camera.up = {0.0f, 1.0f, 0.0f};
    camera.fovy = 45.0f;
    camera.projection = CAMERA_PERSPECTIVE;
}

void CameraController::Update()
{
    float wheel = RaylibWrapper::GetMouseWheelMove();
    if (wheel != 0.0f) {
        camera.fovy -= wheel * 2.0f;
        if (camera.fovy < 10.0f) camera.fovy = 10.0f;
        if (camera.fovy > 90.0f) camera.fovy = 90.0f;
    }
    RaylibWrapper::UpdateCamera(camera, CAMERA_FIRST_PERSON);
}

Camera CameraController::GetCamera() const
{
    return camera;
}

void CameraController::SetCamera(const Camera &cam)
{
    camera = cam;
}
