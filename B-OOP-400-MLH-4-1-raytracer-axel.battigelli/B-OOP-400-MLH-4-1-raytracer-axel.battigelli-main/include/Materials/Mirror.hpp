#pragma once

#include "Core/HitRecord.hpp"
#include "Core/IMaterial.hpp"

namespace Raytracer::Materials
{
    class Mirror : public IMaterial {
      public:
        std::optional<Color> color;
        Color emitted_color;
        double fuzz;

        Mirror(std::optional<Color> color,
                  double fuzz = 0.0,
                  Color emitted_color = Color(0.0, 0.0, 0.0))
            : color(color),
              fuzz(fuzz),
              emitted_color(emitted_color) {}

        virtual Color emitted(const HitRecord &rec, std::default_random_engine &rng) override
        {
            return emitted_color;
        }

        virtual std::optional<std::pair<Color, Ray>> scatter(
            const Ray &r_in, const HitRecord &rec, std::default_random_engine &rng) override
        {
            if (!color)
                return std::nullopt;

            Vector3 reflected = r_in.direction.normalize() - 2 * r_in.direction.normalize().dot(rec.normal) * rec.normal;
            Vector3 scattered_dir = (reflected + fuzz * Vector3::randomUnit(rng)).normalize();

            Ray scattered(rec.point, scattered_dir);
            if (scattered.direction.dot(rec.normal) > 0) {
                return std::make_pair(color.value(), scattered);
            } else {
                return std::nullopt;
            }
        }
    };
} // namespace Raytracer::Materials
