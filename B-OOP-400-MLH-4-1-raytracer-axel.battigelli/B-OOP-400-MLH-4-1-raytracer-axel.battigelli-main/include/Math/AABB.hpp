#pragma once
#include "Math/Ray.hpp"
#include "Math/Vector3.hpp"

namespace Raytracer
{

    class AABB {
      public:
        Vector3 min, max;

        AABB() = default;
        AABB(const Vector3 &min, const Vector3 &max) : min(min), max(max) {}

        [[nodiscard]] bool hit(const Ray &ray, double t_min, double t_max) const
        {
            for (int a = 0; a < 3; ++a) {
                double invD = 1.0 / ray.direction[a];
                double t0 = (min[a] - ray.origin[a]) * invD;
                double t1 = (max[a] - ray.origin[a]) * invD;
                if (invD < 0.0)
                    std::swap(t0, t1);
                t_min = t0 > t_min ? t0 : t_min;
                t_max = t1 < t_max ? t1 : t_max;
                if (t_max <= t_min)
                    return false;
            }
            return true;
        }

        static AABB surrounding_box(const AABB &box0, const AABB &box1)
        {
            return AABB{box0.min.pairwise_min(box1.min), box0.max.pairwise_max(box1.max)};
        }
    };

} // namespace Raytracer
