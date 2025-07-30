#pragma once
#include "Core/IMaterial.hpp"
#include "Math/AABB.hpp"
#include "Math/Ray.hpp"
#include "Math/Vector3.hpp"

#include <optional>

namespace Raytracer
{

    class IPrimitive {
      public:
        [[nodiscard]] virtual std::optional<HitRecord> intersect(
            const Ray &ray, double t_min, double t_max) const = 0;
        virtual std::optional<AABB> boundingBox() const = 0;
        virtual ~IPrimitive() = default;
    };
} // namespace Raytracer
