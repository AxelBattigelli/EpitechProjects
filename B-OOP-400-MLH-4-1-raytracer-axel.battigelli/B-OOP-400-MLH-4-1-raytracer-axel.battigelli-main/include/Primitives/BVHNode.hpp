#pragma once
#include "Core/IPrimitive.hpp"
#include "Math/AABB.hpp"

#include <algorithm>
#include <memory>
#include <vector>

namespace Raytracer::Primitives
{

    class BVHNode : public IPrimitive {
      public:
        std::shared_ptr<BVHNode> left;
        std::shared_ptr<BVHNode> right;
        AABB box;
        std::shared_ptr<IPrimitive> object;

        BVHNode(std::vector<std::shared_ptr<IPrimitive>> &objects);

        [[nodiscard]] std::optional<HitRecord> intersect(
            const Ray &ray, double t_min, double t_max) const override;

        std::optional<AABB> boundingBox() const override { return box; }
    };

} // namespace Raytracer::Primitives
