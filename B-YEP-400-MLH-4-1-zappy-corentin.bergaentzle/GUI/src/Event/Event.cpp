/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Event
*/

#include "Event.hpp"
#include <raymath.h>

Event::Event(CameraController &cam, ParticlesSystem &particles)
    : cam(cam), particles(particles)
{}

Event::~Event()
{}

void Event::HandleEvent()
{
    if (RaylibWrapper::WindowShouldClose())
        RaylibWrapper::CloseWindow();
    if (RaylibWrapper::IsKeyPressed(KEY_SPACE)) {
        Camera came = cam.GetCamera();
        Vector3 dir = Vector3Normalize(Vector3Subtract(came.target, came.position));
        Vector3 spawnPos = {
            came.position.x + dir.x * 2.0f,
            came.position.y + dir.y * 2.0f,
            came.position.z + dir.z * 2.0f
        };
        particles.SpawnParticles(spawnPos);
    }
}
