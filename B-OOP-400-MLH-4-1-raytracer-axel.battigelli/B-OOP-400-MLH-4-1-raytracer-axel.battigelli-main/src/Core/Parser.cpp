#include "Core/Parser.hpp"

#include "Core/IMaterial.hpp"
#include "Exception.hpp"
#include "Materials/FlatColor.hpp"
#include "Materials/Transparency.hpp"
#include "Math/Vector3.hpp"
#include "Primitives/Plane.hpp"
#include "Primitives/Sphere.hpp"
#include "Primitives/SphereBuilder.hpp"
#include "Primitives/PlaneBuilder.hpp"
#include "Primitives/BoxeBuilder.hpp"
#include "Primitives/CylinderBuilder.hpp"
#include "Primitives/ConeBuilder.hpp"
#include "Primitives/TorusBuilder.hpp"
#include "Primitives/PyramideBuilder.hpp"
#include "Primitives/OBJFileBuilder.hpp"
#include "Core/Lights/Sky.hpp"
#include "Core/Lights/Sun.hpp"

#include <libconfig.h++>

#include <fstream>
#include <iostream>
#include <string>

void Raytracer::Parser::readFile(const std::string &filename)
{
    const std::ifstream inputFile(filename);
    if (!inputFile) {
        throw Raytracer::Exception("Error opening file for reading");
    }
    libconfig::Config cfg;
    cfg.readFile(filename.c_str());

    try {
        // --- Camera ---
        const libconfig::Setting &cam = cfg.lookup("camera");
        const libconfig::Setting &res = cam["resolution"];
        const int width = res["width"];
        const int height = res["height"];
        const libconfig::Setting &pos = cam["position"];
        const Vector3 lookFrom(static_cast<double>(pos["x"]),
            static_cast<double>(pos["y"]),
            static_cast<double>(pos["z"]));
        const libconfig::Setting &rot = cam["lookAt"];
        const Vector3 lookAt(static_cast<double>(rot["x"]),
            static_cast<double>(rot["y"]),
            static_cast<double>(rot["z"]));
        const libconfig::Setting &viewUp = cam["viewUp"];
        const Vector3 vup(static_cast<double>(viewUp["x"]),
            static_cast<double>(viewUp["y"]),
            static_cast<double>(viewUp["z"]));
        const auto fov = static_cast<double>(static_cast<float>(cam["fieldOfView"]));
        const int samplePerPixel = static_cast<int>(cam["samplePerPixel"]);
        this->_cam = Raytracer::Camera(width, height, lookFrom, lookAt, vup, fov, samplePerPixel);
    } catch (const Raytracer::ParserException &e) {
        std::cerr << "Camera error: " << e << '\n';
        throw Raytracer::Exception("Fatal error");
    } catch (const std::exception &e) {
        std::cerr << "Camera error: " << e.what() << '\n';
        throw Raytracer::Exception("Fatal error");
    } catch (...) {
        std::cerr << "Unknown error" << "\nInfo to give to the developper: "
                  << std::current_exception().__cxa_exception_type()->name() << '\n';
        throw Raytracer::Exception("Fatal error");
    }

    this->_world = Raytracer::Primitives::List();

    // --- Primitives ---
    if (cfg.exists("primitives")) {
        const libconfig::Setting &primitives = cfg.lookup("primitives");
        for (int i = 0; i < primitives.getLength(); ++i) {
            try {
                const libconfig::Setting &primitive = primitives[i];
                const std::string type = primitive["type"];
                const libconfig::Setting &contentList = primitive["content"];
                std::cout << "Primitive type: " << type << '\n';

                if (type == "sphere") {
                    Raytracer::Primitives::SphereBuilder sphere(contentList);
                    this->_world.add(std::make_shared<Raytracer::Primitives::Sphere>(sphere.build()));
                }
                if (type == "plane") {
                    Raytracer::Primitives::PlaneBuilder plane(contentList);
                    this->_world.add(std::make_shared<Raytracer::Primitives::Plane>(plane.build()));
                }
                if (type == "box") {
                    Raytracer::Primitives::BoxeBuilder boxe(contentList);
                    this->_world.add(std::make_shared<Raytracer::Primitives::Boxe>(boxe.build()));
                }
                if (type == "cylinder") {
                    Raytracer::Primitives::CylinderBuilder cylinder(contentList);
                    this->_world.add(std::make_shared<Raytracer::Primitives::Cylinder>(cylinder.build()));
                }
                if (type == "cone") {
                    Raytracer::Primitives::ConeBuilder cone(contentList);
                    this->_world.add(std::make_shared<Raytracer::Primitives::Cone>(cone.build()));
                }
                if (type == "torus") {
                    Raytracer::Primitives::TorusBuilder torus(contentList);
                    this->_world.add(std::make_shared<Raytracer::Primitives::Torus>(torus.build()));
                }
                if (type == "pyramid") {
                    Raytracer::Primitives::PyramideBuilder pyramide(contentList);
                    this->_world.add(std::make_shared<Raytracer::Primitives::Pyramide>(pyramide.build()));
                }
                if (type == "objfile") {
                    Raytracer::Primitives::OBJFileBuilder OBJFile(contentList);
                    this->_world.add(std::make_shared<Raytracer::Primitives::ObjFile>(OBJFile.build()));
                }
            } catch (const Raytracer::ParserException &e) {
                std::cerr << "Parser error: " << e << '\n';
            } catch (const std::exception &e) {
                std::cerr << "Parser error: " << e.what() << '\n';
            } catch (...) {
                std::cerr << "Unknown error" << "\nInfo to give to the developper: "
                        << std::current_exception().__cxa_exception_type()->name() << '\n';
            }
        }
    }

    // --- Lights ---
    if (cfg.exists("lights")) {
        const libconfig::Setting &primitives = cfg.lookup("lights");
        for (int i = 0; i < primitives.getLength(); ++i) {
            try {
                const libconfig::Setting &primitive = primitives[i];
                const std::string type = primitive["type"];
                const libconfig::Setting &contentList = primitive["content"];
                std::cout << "Light type: " << type << '\n';

                if (type == "ambient") {
                    if (contentList[0].exists("multiplier")) {
                        this->_cam.addLight(std::make_shared<Raytracer::SkyLight>(static_cast<double>(contentList[0]["multiplier"])));
                    } else {
                        this->_cam.addLight(std::make_shared<Raytracer::SkyLight>());
                    }
                }
                if (type == "directional") {
                    if (contentList[0].exists("multiplier")) {
                        if (contentList[0].exists("size")) {
                            this->_cam.addLight(std::make_shared<Raytracer::SunLight>(static_cast<double>(contentList[0]["multiplier"]), static_cast<double>(contentList[0]["size"])));
                        } else {
                            this->_cam.addLight(std::make_shared<Raytracer::SunLight>(static_cast<double>(contentList[0]["multiplier"])));
                        }
                    } else if (contentList[0].exists("size")) {
                        if (contentList[0].exists("multiplier")) {
                            this->_cam.addLight(std::make_shared<Raytracer::SunLight>(static_cast<double>(contentList[0]["multiplier"]), static_cast<double>(contentList[0]["size"])));
                        } else {
                            this->_cam.addLight(std::make_shared<Raytracer::SunLight>(20.0, static_cast<double>(contentList[0]["size"])));
                        }
                    }else {
                        this->_cam.addLight(std::make_shared<Raytracer::SunLight>());
                    }                }
            } catch (const Raytracer::ParserException &e) {
                std::cerr << "Parser error: " << e << '\n';
            } catch (const std::exception &e) {
                std::cerr << "Parser error: " << e.what() << '\n';
            } catch (...) {
                std::cerr << "Unknown error" << "\nInfo to give to the developper: "
                        << std::current_exception().__cxa_exception_type()->name() << '\n';
            }
        }
    }
}
