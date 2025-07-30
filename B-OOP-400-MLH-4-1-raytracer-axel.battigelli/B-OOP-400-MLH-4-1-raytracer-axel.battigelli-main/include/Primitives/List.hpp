#pragma once

#include "Core/IPrimitive.hpp"
#include "Primitives/BVHNode.hpp"

#include <memory>
#include <vector>

namespace Raytracer::Primitives
{
    class List : public IPrimitive {
      private:
        std::vector<std::shared_ptr<IPrimitive>> objects;
        std::shared_ptr<BVHNode> bvh;

      public:
        void clear();
        void add(std::shared_ptr<IPrimitive>);
        std::optional<HitRecord> intersect(
            const Ray &ray, double t_min, double t_max) const override;
        int log() { return objects.size(); };
        void buildBVH();
        std::optional<AABB> boundingBox() const override;
    };
} // namespace Raytracer::Primitives
