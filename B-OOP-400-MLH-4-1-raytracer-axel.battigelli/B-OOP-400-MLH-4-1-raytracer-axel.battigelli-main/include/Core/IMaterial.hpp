#pragma once
#include "Math/Ray.hpp"

#include <optional>
#include <random>

namespace Raytracer
{
    class HitRecord;

    class IMaterial {
      public:
        virtual Color emitted(const HitRecord &rec, std::default_random_engine &rng) = 0;
        virtual std::optional<std::pair<Color, Ray>> scatter(
            const Ray &r_in, const HitRecord &rec, std::default_random_engine &rng) = 0;
        virtual ~IMaterial() = default;
    };
} // namespace Raytracer