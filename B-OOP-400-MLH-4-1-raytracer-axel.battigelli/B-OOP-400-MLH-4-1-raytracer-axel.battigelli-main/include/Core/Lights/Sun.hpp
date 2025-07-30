#pragma once

#include "Core/ILight.hpp"

namespace Raytracer {

    class SunLight : public ILight {
      private:
        Vector3 _direction;
        double _intensity;
        double _size;

      public:
        SunLight(double intensity = 20.0, double size = 0.005,
            Vector3 direction = Vector3(0.7, 1.0, 0.0))
            : _intensity(intensity), _size(size), _direction(direction.normalize()) {}

        Color illuminate(const Ray& ray) const override {
            Vector3 unitDirection = ray.direction.normalize();
            double dot = unitDirection.dot(_direction);
            if (dot > 0.0) {
                double sunFactor = std::exp((dot - 1.0) / this->_size);
                return sunFactor *this->_intensity * Color(1.0, 1.0, 0.9);
            }
            return Color(0.0, 0.0, 0.0);
        }
    };

} // namespace Raytracer
