#pragma once

#include "Core/HitRecord.hpp"
#include "Core/IMaterial.hpp"

namespace Raytracer::Materials
{
    class FlatColor : public IMaterial {
      public:
        std::optional<Color> color;
        Color emitted_color;
        FlatColor(std::optional<Color> color, Color emitted_color = Color(0.0, 0.0, 0.0))
            : color(color), emitted_color(emitted_color) {};

        virtual Color emitted(const HitRecord &rec, std::default_random_engine &rng) override
        {
            return emitted_color;
        }

        virtual std::optional<std::pair<Color, Ray>> scatter(
            const Ray &r_in, const HitRecord &rec, std::default_random_engine &rng) override
        {
            if (this->color)
                return std::make_pair(
                    this->color.value(), Ray(rec.point, rec.normal + Vector3::randomUnit(rng)));
            return std::nullopt;
        }
    };
} // namespace Raytracer::Materials
