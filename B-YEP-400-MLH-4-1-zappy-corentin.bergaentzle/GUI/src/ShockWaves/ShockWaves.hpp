/*
** EPITECH PROJECT, 2025
** ShockWaves
** File description:
** ShockWaves
*/

#pragma once

#include <cmath>
#include "../Wrappers/Raylib/RaylibWrapper.hpp"

class ShockWaves {
    public:
        ShockWaves(Vector3 origin, float maxRadius, float growthSpeed, Color color);
        ~ShockWaves();
        void Update(float delta);
        void Draw() const;
        Vector3 _origin;
        float _radius;
        float _maxRadius;
        float _growthSpeed;
        float _alpha;
        bool _active;
        Color _color;
};
