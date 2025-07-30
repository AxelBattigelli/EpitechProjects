#include "Primitives/Sphere.hpp"
#include "Core/HitRecord.hpp"
#include "Core/IMaterial.hpp"
#include "Math/Ray.hpp"
#include "Math/Vector3.hpp"

#include <optional>
#include <utility>

Raytracer::Primitives::Sphere::Sphere(
    const Vector3 &center, double radius, std::shared_ptr<IMaterial> material)
    : center(center), radius(radius), material(std::move(material))
{}

auto Raytracer::Primitives::Sphere::intersect(const Ray &ray, double t_min, double t_max) const
    -> std::optional<Raytracer::HitRecord>
{
    const Vector3 ocVal = ray.origin - this->center;
    const double aVal = ray.direction.dot(ray.direction);
    const double half_b = ocVal.dot(ray.direction);
    const double cVal = ocVal.dot(ocVal) - (this->radius * this->radius);
    const double discriminant = (half_b * half_b) - (aVal * cVal);

    if (discriminant < 0) {
        return std::nullopt;
    }

    const double sqrtD = std::sqrt(discriminant);
    double root = (-half_b - sqrtD) / aVal;
    if (root < t_min || root > t_max) {
        root = (-half_b + sqrtD) / aVal;
        if (root < t_min || root > t_max) {
            return std::nullopt;
        }
    }

    Raytracer::HitRecord rec;
    rec.t = root;
    rec.point = ray.at(root);
    rec.normal = (rec.point - this->center) / this->radius;
    rec.frontFace = ray.direction.dot(rec.normal) < 0;
    if (!rec.frontFace)
        rec.normal = -rec.normal;
    rec.material = this->material;
    return rec;
}
