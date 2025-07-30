#include "Primitives/List.hpp"

#include "Core/HitRecord.hpp"
#include "Core/IPrimitive.hpp"
#include "Math/Ray.hpp"

#include <iostream>
#include <memory>
#include <optional>
#include <utility>

void Raytracer::Primitives::List::clear()
{
    this->objects.clear();
    this->bvh.reset();
}
void Raytracer::Primitives::List::add(std::shared_ptr<IPrimitive> added)
{
    this->objects.push_back(std::move(added));
    this->bvh.reset();
}
void Raytracer::Primitives::List::buildBVH()
{
    std::vector<std::shared_ptr<IPrimitive>> valid_objects;

    // Collect all objects with a valid bounding box
    for (const auto &object : objects) {
        auto box = object->boundingBox();
        if (box) {
            valid_objects.push_back(object);
        }
    }

    // Construct a BVH with the valid objects
    if (!valid_objects.empty()) {
        this->bvh = std::make_shared<Raytracer::Primitives::BVHNode>(valid_objects);
    }
}
auto Raytracer::Primitives::List::intersect(const Ray &ray, double t_min, double t_max) const
    -> std::optional<Raytracer::HitRecord>
{
    std::optional<HitRecord> closest_hit;

    // First, check the BVH for intersections with the valid objects
    if (bvh) {
        closest_hit = bvh->intersect(ray, t_min, t_max);
    }

    // Then, check for intersections with objects that do not have AABBs (like planes)
    for (const auto &object : objects) {
        auto box = object->boundingBox();
        if (!box) {
            // If the object has no bounding box, check for intersection directly (like a plane)
            auto hit = object->intersect(ray, t_min, (closest_hit) ? closest_hit->t : t_max);
            if (hit) {
                closest_hit = hit;
            }
        }
    }

    return closest_hit;
}

auto Raytracer::Primitives::List::boundingBox() const -> std::optional<AABB>
{
    AABB combined_box;
    bool first_box = true;

    for (const auto &object : objects) {
        auto box = object->boundingBox();
        if (box) {
            if (first_box) {
                combined_box = *box;
                first_box = false;
            } else {
                combined_box = AABB::surrounding_box(combined_box, *box);
            }
        } else {
            return std::nullopt;
        }
    }

    return first_box ? std::nullopt : std::make_optional(combined_box);
}
