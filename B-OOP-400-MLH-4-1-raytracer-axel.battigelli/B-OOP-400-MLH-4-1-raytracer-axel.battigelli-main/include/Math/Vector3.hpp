#pragma once

#include <cmath>
#include <random>
#include <stdexcept>

namespace Raytracer
{
    class Vector3 {
      public:
        double x, y, z;
        constexpr Vector3() : x(0), y(0), z(0) {};
        constexpr Vector3(double x, double y, double z) : x(x), y(y), z(z) {};
        template<typename RNG> static auto random(RNG &rng, double min, double max) -> Vector3
        {
            std::uniform_real_distribution<double> dis(min, max);
            return {dis(rng), dis(rng), dis(rng)};
        }
        template<typename RNG> static auto randomUnit(RNG &rng) -> Vector3
        {
            while (true) {
                auto p = Vector3::random(rng, -1, 1);
                auto lensq = p.sqr_length();
                if (1e-160 < lensq && lensq <= 1.0)
                    return p / sqrt(lensq);
            }
        }
        constexpr auto operator+(const Vector3 &rhs) const -> Vector3
        {
            return {x + rhs.x, y + rhs.y, z + rhs.z};
        }
        constexpr auto operator+=(const Vector3 &rhs) -> Vector3 &
        {
            this->x += rhs.x;
            this->y += rhs.y;
            this->z += rhs.z;
            return *this;
        }
        constexpr auto operator-(const Vector3 &rhs) const -> Vector3
        {
            return {x - rhs.x, y - rhs.y, z - rhs.z};
        }
        constexpr auto operator*(double scalar) const -> Vector3
        {
            return {x * scalar, y * scalar, z * scalar};
        }
        constexpr auto operator/=(double scalar) -> Vector3 &
        {
            x /= scalar;
            y /= scalar;
            z /= scalar;
            return *this;
        }
        constexpr auto operator/(double scalar) const -> Vector3
        {
            return {x / scalar, y / scalar, z / scalar};
        }
        constexpr auto operator-() const -> Vector3 { return *this * -1; }
        [[nodiscard]] constexpr auto dot(const Vector3 &rhs) const -> double
        {
            return (x * rhs.x) + (y * rhs.y) + (z * rhs.z);
        }

        [[nodiscard]] constexpr auto cross(const Vector3 &rhs) const -> Vector3
        {
            return {
                (y * rhs.z) - (z * rhs.y), (z * rhs.x) - (x * rhs.z), (x * rhs.y) - (y * rhs.x)};
        }

        [[nodiscard]] constexpr auto pairwise_mul(const Vector3 &rhs) const -> Vector3
        {
            return {x * rhs.x, y * rhs.y, z * rhs.z};
        }
        [[nodiscard]] constexpr auto normalize() const -> Vector3
        {
            return (*this) / this->length();
        }
        [[nodiscard]] constexpr auto length() const -> double
        {
            return sqrt((x * x) + (y * y) + (z * z));
        }
        [[nodiscard]] constexpr auto sqr_length() const -> double
        {
            return (x * x) + (y * y) + (z * z);
        }
        double operator[](int index) const
        {
            if (index == 0)
                return x;
            if (index == 1)
                return y;
            if (index == 2)
                return z;
            throw std::out_of_range("Index must be 0, 1, or 2 for Vector3.");
        }

        double &operator[](int index)
        {
            if (index == 0)
                return x;
            if (index == 1)
                return y;
            if (index == 2)
                return z;
            throw std::out_of_range("Index must be 0, 1, or 2 for Vector3.");
        }
        Vector3 pairwise_max(const Vector3 &v2) const
        {
            return {std::max(this->x, v2.x), std::max(this->y, v2.y), std::max(this->z, v2.z)};
        }

        Vector3 pairwise_min(const Vector3 &v2) const
        {
            return {std::min(this->x, v2.x), std::min(this->y, v2.y), std::min(this->z, v2.z)};
        }

        Vector3 max_by_length(const Vector3 &v2) const
        {
            if (this->sqr_length() > v2.sqr_length())
                return *this;
            return v2;
        }
    };

    constexpr auto operator*(double scalar, const Vector3 &rhs) -> Vector3
    {
        return rhs * scalar;
    }

    constexpr auto operator/(double scalar, const Vector3 &rhs) -> Vector3
    {
        return rhs / scalar;
    }

    using Color = Vector3;
    using Point3 = Vector3;
} // namespace Raytracer
