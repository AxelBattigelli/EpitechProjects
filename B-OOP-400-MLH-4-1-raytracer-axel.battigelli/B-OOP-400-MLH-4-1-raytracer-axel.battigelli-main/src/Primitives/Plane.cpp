#include "Primitives/Plane.hpp"
#include "Core/HitRecord.hpp"
#include "Math/Ray.hpp"

#include <memory>
#include <optional>

Raytracer::Primitives::Plane::Plane(
    const Vector3 &normal, double position, std::shared_ptr<IMaterial> material)
    : normal(normal.normalize()), position(position), material(std::move(material))
{}

auto Raytracer::Primitives::Plane::intersect(const Ray &ray, double t_min, double t_max) const
    -> std::optional<Raytracer::HitRecord>
{
    const double denom = this->normal.dot(ray.direction);

    if (std::abs(denom) < 1e-8) {
        return std::nullopt;
    }

    const double tVal = (this->position - this->normal.dot(ray.origin)) / denom;

    if (tVal < t_min || tVal > t_max) {
        return std::nullopt;
    }

    Raytracer::HitRecord rec;
    rec.t = tVal;
    rec.point = ray.at(tVal);
    rec.normal = this->normal;
    rec.frontFace = ray.direction.dot(this->normal) < 0;
    if (!rec.frontFace)
        rec.normal = -rec.normal;
    rec.material = this->material;
    return rec;
}
