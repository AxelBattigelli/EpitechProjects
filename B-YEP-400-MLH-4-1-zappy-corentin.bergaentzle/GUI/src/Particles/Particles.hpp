/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Particles
*/

#pragma once
#include "../Wrappers/Raylib/RaylibWrapper.hpp"

class ParticlesSystem {
public:
    static constexpr int MAX_PARTICLES = 100;

    ParticlesSystem();
    ~ParticlesSystem();

    void SpawnParticles(Vector3 origin);
    void Update(float deltaTime);
    void Draw() const;

private:
    Vector3 positions[MAX_PARTICLES];
    Vector3 velocities[MAX_PARTICLES];
    float lifes[MAX_PARTICLES];
    Color colors[MAX_PARTICLES];
};

