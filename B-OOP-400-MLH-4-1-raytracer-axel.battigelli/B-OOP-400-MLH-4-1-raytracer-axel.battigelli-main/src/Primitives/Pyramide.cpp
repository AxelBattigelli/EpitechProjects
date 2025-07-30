#include "Primitives/Pyramide.hpp"
#include "Core/HitRecord.hpp"
#include "Math/Ray.hpp"

#include <cmath>
#include <optional>
#include <vector>
#include <memory>

Raytracer::Primitives::Pyramide::Pyramide(Point3 center, Vector3 upNormal, double baseLength, double baseWidth, double height, std::shared_ptr<IMaterial> material)
    : center(center),
      upNormal(upNormal),
      baseLength(baseLength),
      baseWidth(baseWidth),
      height(height),
      material(std::move(material)) {}

std::optional<Raytracer::HitRecord> Raytracer::Primitives::Pyramide::intersect(const Ray &ray, double t_min, double t_max) const
{
    auto rayOrigin = ray.origin;
    auto rayDir = ray.direction;

    Vector3 w = upNormal.normalize();
    Vector3 u = (std::abs(w.x) > 0.9 ? Vector3{0, 1, 0} : Vector3{1, 0, 0}).cross(w).normalize();
    Vector3 v = w.cross(u);

    double halfBase = baseLength / 2.0;
    Point3 p0 = this->center - u * halfBase;
    Point3 p1 = this->center + u * halfBase;
    Point3 p2 = this->center + v * this->baseLength * std::sqrt(3) / 2.0;

    Point3 apex = this->center + w * this->height;

    struct TriangleFace { Point3 a, b, c; };
    std::vector<TriangleFace> faces = {
        {p0, p1, p2},       // base
        {p0, p1, apex},
        {p1, p2, apex},
        {p2, p0, apex}
    };

    std::optional<HitRecord> closestHit;
    double closestT = t_max;

    auto intersect_triangle = [&](const Point3 &a, const Point3 &b, const Point3 &c) -> std::optional<HitRecord> {
        Vector3 edge1 = b - a;
        Vector3 edge2 = c - a;
        Vector3 h = rayDir.cross(edge2);
        double det = edge1.dot(h);

        if (std::abs(det) < 1e-8)
            return std::nullopt;

        double invDet = 1.0 / det;
        Vector3 s = rayOrigin - a;
        double u = s.dot(h) * invDet;
        if (u < 0.0 || u > 1.0)
            return std::nullopt;

        Vector3 q = s.cross(edge1);
        double v = rayDir.dot(q) * invDet;
        if (v < 0.0 || u + v > 1.0)
            return std::nullopt;

        double t = edge2.dot(q) * invDet;
        if (t < t_min || t > t_max)
            return std::nullopt;

        Point3 hitPoint = ray.at(t);
        Vector3 normal = edge1.cross(edge2).normalize();
        if (normal.dot(rayDir) > 0)
            normal = -normal;

        HitRecord rec;
        rec.t = t;
        rec.point = hitPoint;
        rec.normal = normal;
        rec.material = this->material;
        return rec;
    };

    for (const auto &face : faces) {
        auto hit = intersect_triangle(face.a, face.b, face.c);
        if (hit && hit->t < closestT) {
            closestT = hit->t;
            closestHit = hit;
        }
    }
    return closestHit;
}

std::optional<Raytracer::AABB> Raytracer::Primitives::Pyramide::boundingBox() const
{
    Vector3 w = upNormal.normalize();
    Vector3 u = (std::abs(w.x) > 0.9 ? Vector3{0, 1, 0} : Vector3{1, 0, 0}).cross(w).normalize();
    Vector3 v = w.cross(u);

    double halfBase = this->baseLength / 2.0;
    Point3 p0 = this->center - u * halfBase;
    Point3 p1 = this->center + u * halfBase;
    Point3 p2 = this->center + v * this->baseLength * std::sqrt(3) / 2.0;
    Point3 apex = this->center + w * this->height;
    auto minVec = p0;
    auto maxVec = p0;

    auto expand = [&](const Point3 &p) {
        minVec = {
            std::min(minVec.x, p.x),
            std::min(minVec.y, p.y),
            std::min(minVec.z, p.z)
        };
        maxVec = {
            std::max(maxVec.x, p.x),
            std::max(maxVec.y, p.y),
            std::max(maxVec.z, p.z)
        };
    };

    expand(p1);
    expand(p2);
    expand(apex);

    return AABB{minVec, maxVec};
}
