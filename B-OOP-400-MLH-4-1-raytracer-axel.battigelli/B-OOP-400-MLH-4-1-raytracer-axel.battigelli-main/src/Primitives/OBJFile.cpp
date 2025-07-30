/*
** EPITECH PROJECT, 2025
** B-OOP-400-MLH-4-1-raytracer-axel.battigelli
** File description:
** OBJFile
*/

#include "Primitives/OBJFile.hpp"
#include "Exception.hpp"

std::optional<Raytracer::HitRecord> Raytracer::Primitives::Triangle::intersect(const Ray &ray, 
    double t_min, double t_max) const 
{
    Vector3 e1 = this->_v1 - this->_v0;
    Vector3 e2 = this->_v2 - this->_v0;
    Vector3 h = ray.direction.cross(e2);
    double a = e1.dot(h);

    if (a > -std::numeric_limits<double>::epsilon()
        && a < std::numeric_limits<double>::epsilon()) {
        return std::nullopt;
    }

    double f = 1.0 / a;
    Vector3 s = ray.origin - this->_v0;
    double u = f * s.dot(h);

    if (u < 0.0 || u > 1.0) {
        return std::nullopt;
    }

    Vector3 q = s.cross(e1);
    double v = f * ray.direction.dot(q);

    if (v < 0.0 || u + v > 1.0) {
        return std::nullopt;
    }

    double t = f * e2.dot(q);
    if (t > t_min && t < t_max) {
        Vector3 normal = (1 - u - v) * this->_n0 + u * this->_n1 + v * this->_n2;

        HitRecord hit;
        hit.point = ray.origin + ray.direction * t;
        hit.normal = normal.normalize();
        hit.t = t;
        hit.material = this->_mat;
        return hit;
    }

    return std::nullopt;
}

std::optional<Raytracer::AABB> Raytracer::Primitives::Triangle::boundingBox() const
{
    Vector3 minVec = Vector3(std::min({this->_v0.x, this->_v1.x, this->_v2.x}), std::min({this->_v0.y, this->_v1.y, this->_v2.y}),
    std::min({this->_v0.z, this->_v1.z, this->_v2.z}));

    Vector3 maxVec = Vector3(std::max({this->_v0.x, this->_v1.x, this->_v2.x}), std::max({this->_v0.y, this->_v1.y, this->_v2.y}),
        std::max({this->_v0.z, this->_v1.z, this->_v2.z}));

    return AABB(minVec, maxVec);
}

bool Raytracer::Primitives::ObjFile::parseFaceVertexNormal(std::istringstream &ss, int &vIdx, int &nIdx)
{
    std::string vertexData;
    ss >> vertexData;

    if (vertexData == "")
        return false;

    std::replace(
        vertexData.begin(), vertexData.end(), '/', ' ');

    std::istringstream vertexStream(vertexData);

    vertexStream >> vIdx;
    vertexStream.ignore(1);
    vertexStream >> nIdx;
    return true;
}

bool Raytracer::Primitives::ObjFile::loadFromFile(Vector3 scale, Vector3 translate, const std::string &filename)
{
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Failed to open file: " << filename << std::endl;
        throw ParserException("Failed to open file");
    }

    std::string line;
    while (std::getline(file, line)) {
        std::istringstream ss(line);
        std::string prefix;
        ss >> prefix;

        if (prefix == "v") {
            double x, y, z;
            ss >> x >> y >> z;
            _vertices.push_back(Vector3(x * scale.x + translate.x,
            (y * scale.y) + translate.y, (z * scale.z) + translate.z));
        } else if (prefix == "vn") {
            double nx, ny, nz;
            ss >> nx >> ny >> nz;
            this->_normals.push_back(Vector3(nx, ny, nz));
        } else if (prefix == "f") {
            std::vector<int> vertexIndices;
            std::vector<int> normalIndices;
            std::string vertexToken;
            while (ss >> vertexToken) {
                std::replace(vertexToken.begin(), vertexToken.end(), '/', ' ');
                std::istringstream vss(vertexToken);

                int vIdx = 0, dummy = 0, nIdx = 0;
                vss >> vIdx >> dummy >> nIdx;

                vertexIndices.push_back(vIdx - 1);
                normalIndices.push_back(nIdx - 1);
            }
            if (vertexIndices.size() == 3) {
                this->_objects.push_back(std::make_shared<Triangle>(this->_mat,
                this->_vertices[vertexIndices[0]], this->_vertices[vertexIndices[1]],
                this->_vertices[vertexIndices[2]], this->_normals[normalIndices[0]],
                this->_normals[normalIndices[1]], this->_normals[normalIndices[2]]));
            } else if (vertexIndices.size() == 4) {
                this->_objects.push_back(std::make_shared<Triangle>(this->_mat,
                this->_vertices[vertexIndices[0]], this->_vertices[vertexIndices[1]],
                this->_vertices[vertexIndices[2]], this->_normals[normalIndices[0]],
                this->_normals[normalIndices[1]], this->_normals[normalIndices[2]]));
                this->_objects.push_back(std::make_shared<Triangle>(this->_mat,
                this->_vertices[vertexIndices[0]], this->_vertices[vertexIndices[2]],
                this->_vertices[vertexIndices[3]], this->_normals[normalIndices[0]],
                this->_normals[normalIndices[2]], this->_normals[normalIndices[3]]));
            } else {
                std::cerr << "Unsupported polygon with " << vertexIndices.size()
                    << " vertices\n";
            }
        }
    }

    file.close();

    this->_bvhRoot = std::make_shared<Raytracer::Primitives::BVHNode>(this->_objects);
    return true;
}