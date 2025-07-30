#include "Primitives/Cylinder.hpp"

#include "Core/HitRecord.hpp"
#include "Math/Ray.hpp"
#include <cmath>
#include <optional>

Raytracer::Primitives::Cylinder::Cylinder(Point3 center, Vector3 upNormal, double length, double radius, std::shared_ptr<IMaterial> material)
    : center(center),
      upNormal(upNormal.normalize()),
      length(length),
      radius(radius),
      material(std::move(material)) {}

std::optional<Raytracer::HitRecord> Raytracer::Primitives::Cylinder::intersect(const Ray &ray, double t_min, double t_max) const {
    Vector3 const ca = upNormal * length;
    Vector3 const base = center - upNormal * (length / 2);
    Vector3 const oc = ray.origin - base;

    Vector3 const caNorm = ca.normalize();
    Vector3 const rd = ray.direction;
    Vector3 const ocNorm = oc;

    double const  caDotRd = caNorm.dot(rd);
    double const  caDotOc = caNorm.dot(ocNorm);

    Vector3 const d = rd - caNorm * caDotRd;
    Vector3 const o = ocNorm - caNorm * caDotOc;

    double const  a = d.sqr_length();
    double const  b = 2.0 * d.dot(o);
    double const  c = o.sqr_length() - radius * radius;

    double const  discriminant = (b * b) - (4 * a * c);
    double t_side = std::numeric_limits<double>::infinity();
    HitRecord rec_side;

    if (discriminant >= 0) {
        double const sqrtD = std::sqrt(discriminant);
        double t = (-b - sqrtD) / (2 * a);

        if (t < t_min || t > t_max) {
            t = (-b + sqrtD) / (2 * a);
        }

        if (t >= t_min && t <= t_max) {
            Vector3 const hit = ray.at(t);
            double const y = caNorm.dot(hit - base);
            if (y >= 0 && y <= length) {
                Vector3 const proj = base + caNorm * y;
                Vector3 const normal = (hit - proj).normalize();

                rec_side.t = t;
                rec_side.point = hit;
                rec_side.normal = normal;
                rec_side.material = material;
                t_side = t;
            }
        }
    }

    auto intersect_cap = [&](Vector3 cap_center, Vector3 normal) -> std::optional<HitRecord> {
        double const denom = normal.dot(rd);
        if (std::abs(denom) < 1e-6)
            return std::nullopt;

        double const t = (cap_center - ray.origin).dot(normal) / denom;
        if (t < t_min || t > t_max)
            return std::nullopt;

        Vector3 const p = ray.at(t);
        if ((p - cap_center).sqr_length() > radius * radius)
            return std::nullopt;

        HitRecord rec;
        rec.t = t;
        rec.point = p;
        rec.normal = normal;
        rec.material = material;
        return rec;
    };

    auto bottom_hit = intersect_cap(base, -caNorm);
    auto top_hit = intersect_cap(base + ca, caNorm);
    std::optional<HitRecord> final_hit;
    if (bottom_hit && (!final_hit || bottom_hit->t < final_hit->t))
        final_hit = bottom_hit;
    if (top_hit && (!final_hit || top_hit->t < final_hit->t))
        final_hit = top_hit;
    if (t_side < std::numeric_limits<double>::infinity() && (!final_hit || t_side < final_hit->t))
        final_hit = rec_side;
    return final_hit;
}

std::optional<Raytracer::AABB> Raytracer::Primitives::Cylinder::boundingBox() const {
    Vector3 const axis = upNormal.normalize();
    Vector3 ortho1, ortho2;

    if (std::abs(axis.x) > 0.9)
        ortho1 = Vector3(0, 1, 0).cross(axis).normalize();
    else
        ortho1 = Vector3(1, 0, 0).cross(axis).normalize();
    ortho2 = axis.cross(ortho1).normalize();

    ortho1 = ortho1 * this->radius;
    ortho2 = ortho2 * this->radius;

    Vector3 halfHeight = axis * (this->length * 0.5);
    std::vector<Point3> points = {
        center + halfHeight + ortho1 + ortho2,
        center + halfHeight + ortho1 - ortho2,
        center + halfHeight - ortho1 + ortho2,
        center + halfHeight - ortho1 - ortho2,
        center - halfHeight + ortho1 + ortho2,
        center - halfHeight + ortho1 - ortho2,
        center - halfHeight - ortho1 + ortho2,
        center - halfHeight - ortho1 - ortho2,
    };

    Point3 min = points[0];
    Point3 max = points[0];
    for (const auto& p : points) {
        min = min.pairwise_min(p);
        max = max.pairwise_max(p);
    }

    return AABB(min, max);
}
