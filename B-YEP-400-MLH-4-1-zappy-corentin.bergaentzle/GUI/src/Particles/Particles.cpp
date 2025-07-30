/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Particles
*/

#include "Particles.hpp"
#include <cstdlib>
#include <cmath>

ParticlesSystem::ParticlesSystem()
{
    for (int i = 0; i < MAX_PARTICLES; ++i) {
        lifes[i] = 0.0f;
        positions[i] = {0, 0, 0};
        velocities[i] = {0, 0, 0};
        colors[i] = {255, 255, 255, 255};
    }
}

ParticlesSystem::~ParticlesSystem()
{}

void ParticlesSystem::SpawnParticles(Vector3 origin)
{
    for (int i = 0; i < MAX_PARTICLES; i++) {
        float angle = (static_cast<float>(i) / MAX_PARTICLES) * 2 * PI;
        positions[i] = origin;
        float speed = 1.5f + static_cast<float>(rand() % 100) / 100.0f;
        velocities[i] = { cosf(angle) * speed, 1.5f + static_cast<float>(rand() % 100) / 100.0f, sinf(angle) * speed };
        lifes[i] = 1.0f;
        colors[i] = { static_cast<unsigned char>(rand() % 256), static_cast<unsigned char>(rand() % 256), static_cast<unsigned char>(rand() % 256), 255 };
    }
}

void ParticlesSystem::Update(float deltaTime)
{
    for (int i = 0; i < MAX_PARTICLES; i++) {
        if (lifes[i] > 0.0f) {
            positions[i].x += velocities[i].x * deltaTime;
            positions[i].y += velocities[i].y * deltaTime;
            positions[i].z += velocities[i].z * deltaTime;
            velocities[i].y -= 2.5f * deltaTime;
            lifes[i] -= deltaTime;
        }
    }
}

void ParticlesSystem::Draw() const
{
    for (int i = 0; i < MAX_PARTICLES; i++) {
        if (lifes[i] > 0.0f)
            RaylibWrapper::DrawSphere(positions[i], 0.02f, RaylibWrapper::Fade(colors[i], lifes[i]));
    }
}