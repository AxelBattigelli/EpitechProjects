#include "Primitives/BVHNode.hpp"

#include "Core/HitRecord.hpp"
#include "Math/AABB.hpp"
#include "Math/Ray.hpp"

#include <algorithm>
#include <cstdlib> // For random shuffling
#include <iostream>
#include <memory>
#include <optional>
#include <vector>

namespace Raytracer::Primitives
{

    // Constructor for BVHNode that splits the objects into left and right subtrees
    BVHNode::BVHNode(std::vector<std::shared_ptr<IPrimitive>> &objects)
    {
        // If there are fewer than 2 objects, make this a leaf node
        if (objects.size() == 1) {
            left = right = nullptr;
            box = objects[0]->boundingBox().value(); // Set the bounding box for the leaf node
            this->object = objects[0];
        } else {
            // Split objects into two halves based on the median
            int axis = std::rand() % 3; // Randomly choose the axis (X, Y, or Z)

            // Sort objects along the chosen axis
            std::sort(objects.begin(), objects.end(),
                [axis](
                    const std::shared_ptr<IPrimitive> &a, const std::shared_ptr<IPrimitive> &b) {
                    auto box_a = a->boundingBox();
                    auto box_b = b->boundingBox();
                    return box_a->min[axis] < box_b->min[axis]; // Compare by the chosen axis
                });

            // Split the objects into two halves
            size_t mid = objects.size() / 2;
            std::vector<std::shared_ptr<IPrimitive>> left_objects(
                objects.begin(), objects.begin() + mid);
            std::vector<std::shared_ptr<IPrimitive>> right_objects(
                objects.begin() + mid, objects.end());

            // Create child nodes
            left = std::make_shared<BVHNode>(left_objects);
            right = std::make_shared<BVHNode>(right_objects);

            // Combine the bounding boxes of the left and right child nodes
            auto left_box = left->boundingBox();
            auto right_box = right->boundingBox();
            if (left_box && right_box) {
                box = AABB::surrounding_box(*left_box, *right_box); // Combine the boxes
            }
        }
    }

    // Perform intersection tests with this BVHNode
    std::optional<HitRecord> BVHNode::intersect(const Ray &ray, double t_min, double t_max) const
    {
        // First, check if the ray intersects the bounding box of this node
        if (!box.hit(ray, t_min, t_max)) {
            return std::nullopt; // No intersection with the bounding box, so return no hit
        }
        if (this->object) {
            return this->object->intersect(ray, t_min, t_max);
        }

        // Check for intersections with the left and right child nodes, if they exist
        std::optional<HitRecord> hit_left =
            left ? left->intersect(ray, t_min, t_max) : std::nullopt;
        std::optional<HitRecord> hit_right =
            right ? right->intersect(ray, t_min, t_max) : std::nullopt;

        // Combine the results from the left and right child nodes
        std::optional<HitRecord> closest_hit = std::nullopt;
        if (hit_left && hit_right) {
            closest_hit = hit_left->t < hit_right->t ? hit_left : hit_right;
        } else {
            closest_hit = hit_left ? hit_left : hit_right;
        }

        return closest_hit;
    }

} // namespace Raytracer::Primitives
