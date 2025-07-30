/*
** EPITECH PROJECT, 2025
** ShockWaves
** File description:
** ShockWaves
*/

#include "ShockWaves.hpp"

ShockWaves::ShockWaves(Vector3 origin, float maxRadius, float growthSpeed, Color color)
{
    _origin = origin;
    _radius = 0.0f;
    _maxRadius = maxRadius;
    _growthSpeed = growthSpeed;
    _alpha = 1.0f;
    _active = true;
    _color = color;
}

ShockWaves::~ShockWaves()
{
}

void ShockWaves::Update(float delta)
{
    if (!_active) {
        return;
    }
    _radius += _growthSpeed * delta;
    _alpha = 1.0f - (_radius / _maxRadius);
    if (_radius >= _maxRadius) {
        _active = false;
    }
}

void ShockWaves::Draw() const
{
    if (!_active) {
        return;
    }
    const int segments = 64;
    float angleStep = 2 * PI / segments;
    Color color = RaylibWrapper::Fade(_color, _alpha);

    for (int i = 0; i < segments; ++i) {
        float angle1 = i * angleStep;
        float angle2 = (i + 1) * angleStep;

        Vector3 p1 = {
            _origin.x + _radius * cosf(angle1),
            _origin.y,
            _origin.z + _radius * sinf(angle1)
        };
        Vector3 p2 = {
            _origin.x + _radius * cosf(angle2),
            _origin.y,
            _origin.z + _radius * sinf(angle2)
        };

        RaylibWrapper::DrawLine3D(p1, p2, color);
    }
}
