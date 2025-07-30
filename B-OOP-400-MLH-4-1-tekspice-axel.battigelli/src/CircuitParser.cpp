#include "CircuitParser.hpp"
#include "Circuit.hpp"
#include "Component/AndComponent.hpp"
#include "Component/ClockComponent.hpp"
#include "Component/FalseComponent.hpp"
#include "Component/Gate2716.hpp"
#include "Component/Gate4001.hpp"
#include "Component/Gate4008.hpp"
#include "Component/Gate4011.hpp"
#include "Component/Gate4013.hpp"
#include "Component/Gate4017.hpp"
#include "Component/Gate4030.hpp"
#include "Component/Gate4040.hpp"
#include "Component/Gate4069.hpp"
#include "Component/Gate4071.hpp"
#include "Component/Gate4081.hpp"
#include "Component/Gate4094.hpp"
#include "Component/Gate4512.hpp"
#include "Component/Gate4514.hpp"
#include "Component/Gate4801.hpp"
#include "Component/InputComponent.hpp"
#include "Component/LoggerComponent.hpp"
#include "Component/NotComponent.hpp"
#include "Component/OrComponent.hpp"
#include "Component/OutputComponent.hpp"
#include "Component/TrueComponent.hpp"
#include "Component/XorComponent.hpp"
#include "Exception.hpp"
#include "IComponent.hpp"
#include <unordered_map>
#include <algorithm>
#include <cctype>
#include <cstddef>
#include <format>
#include <ios>
#include <iostream>
#include <istream>
#include <memory>
#include <ranges>
#include <sstream>
#include <string>
#include <utility>

inline static std::string &ltrim(std::string &s)
{
    s.erase(
        s.begin(), std::ranges::find_if(s, [](unsigned char ch) { return !std::isspace(ch); }));
    return s;
}

inline static std::string &rtrim(std::string &s)
{
    s.erase(std::ranges::find_if(
                std::ranges::reverse_view(s), [](unsigned char ch) { return !std::isspace(ch); })
                .base(),
        s.end());
    return s;
}

std::unique_ptr<nts::IComponent> nts::CircuitParser::createComponent(const std::string &type)
{
    if (type == "input")
        return std::make_unique<nts::component::InputComponent>();
    if (type == "output")
        return std::make_unique<nts::component::OutputComponent>();
    if (type == "and")
        return std::make_unique<nts::component::AndComponent>();
    if (type == "false")
        return std::make_unique<nts::component::FalseComponent>();
    if (type == "clock")
        return std::make_unique<nts::component::ClockComponent>();
    if (type == "2716")
        return std::make_unique<nts::component::Gate2716>();
    if (type == "4001")
        return std::make_unique<nts::component::Gate4001>();
    if (type == "4008")
        return std::make_unique<nts::component::Gate4008>();
    if (type == "4011")
        return std::make_unique<nts::component::Gate4011>();
    if (type == "4013")
        return std::make_unique<nts::component::Gate4013>();
    if (type == "4017")
        return std::make_unique<nts::component::Gate4017>();
    if (type == "4030")
        return std::make_unique<nts::component::Gate4030>();
    if (type == "4040")
        return std::make_unique<nts::component::Gate4040>();
    if (type == "4069")
        return std::make_unique<nts::component::Gate4069>();
    if (type == "4071")
        return std::make_unique<nts::component::Gate4071>();
    if (type == "4081")
        return std::make_unique<nts::component::Gate4081>();
    if (type == "4094")
        return std::make_unique<nts::component::Gate4094>();
    if (type == "4512")
        return std::make_unique<nts::component::Gate4512>();
    if (type == "4514")
        return std::make_unique<nts::component::Gate4514>();
    if (type == "4801")
        return std::make_unique<nts::component::Gate4801>();
    if (type == "logger")
        return std::make_unique<nts::component::LoggerComponent>();
    if (type == "not")
        return std::make_unique<nts::component::NotComponent>();
    if (type == "or")
        return std::make_unique<nts::component::OrComponent>();
    if (type == "true")
        return std::make_unique<nts::component::TrueComponent>();
    if (type == "xor")
        return std::make_unique<nts::component::XorComponent>();

    throw nts::Exception(std::format("Unknown component '{}'", type));
}

nts::Circuit nts::CircuitParser::parse(std::istream &stream)
{
    std::string buf;
    std::stringstream line;
    nts::Circuit circuit;
    std::unordered_map<std::string, std::unique_ptr<nts::IComponent>> elements;

    while (!stream.eof()) {
        if (!stream)
            throw nts::Exception("Error reading stream");
        if (!std::getline(stream, buf) && !stream.eof())
            throw nts::Exception("Error reading stream");
        buf.erase(std::ranges::find(buf, '#'), buf.cend());
        ltrim(rtrim(buf));
        if (buf.empty())
            continue;
        if (buf == ".chipsets:") {
            this->_state = State::Chipsets;
            // std::cout << "CHIPSETS\n";
            continue;
        }
        if (buf == ".links:") {
            this->_state = State::Links;
            // std::cout << "LINKS\n";
            continue;
        }
        line = std::stringstream(buf);
        if (this->_state == State::Chipsets) {
            std::string type;
            std::string name;
            line >> type >> name;
            // std::cerr << "New chip named '" << name << "' of type '" << type << "'\n";
            if (name == "")
                throw nts::Exception(std::format("Can't have a chip without name!"));
            if (elements.contains(name))
                throw nts::Exception(std::format("Chip '{}' already exists", name));
            elements.insert_or_assign(name, nts::CircuitParser::createComponent(type));
            name.clear();
            line >> name;
            if (name != "")
                throw nts::Exception(std::format("Bad Syntax"));
            continue;
        }
        if (this->_state == State::Links) {
            std::string leftString;
            std::string rightString;
            line.setf(std::ios_base::skipws);
            line >> leftString;
            line >> rightString;
            std::stringstream left(leftString);
            std::stringstream right(rightString);
            std::string leftName;
            std::string rightName;
            if (std::find(leftString.cbegin(), leftString.cend(), ':') == leftString.cend())
                throw nts::Exception("Invalid syntax");
            if (std::find(rightString.cbegin(), rightString.cend(), ':') == rightString.cend())
                throw nts::Exception("Invalid syntax");
            std::getline(left, leftName, ':');
            std::string leftPinString;
            left >> leftPinString;
            if (leftPinString == "")
                throw nts::Exception(std::format("Missing link in {}", leftPinString));
            std::getline(right, rightName, ':');
            std::string rightPinString;
            right >> rightPinString;
            if (rightPinString == "")
                throw nts::Exception(std::format("Missing link in {}", rightPinString));
            // std::cerr << "New link between chip '" << leftName << "' pin " << left_pin
            //           << " and chip '" << rightName << "' pin " << right_pin << "\n";
            if (!elements.contains(leftName))
                throw nts::Exception(std::format("Chip '{}' does not exist", leftName));
            if (!elements.contains(rightName))
                throw nts::Exception(std::format("Chip '{}' does not exist", rightName));
            elements.at(leftName)->setLink(
                std::stoull(leftPinString), *elements.at(rightName), std::stoull(rightPinString));
            continue;
        }
        throw nts::Exception("Bad input");
    }
    for (auto &elem : elements) {
        circuit.addComponent(elem.first, std::move(elem.second));
    }
    if (circuit._components.empty()) {
        throw nts::Exception("No components in the circuit!!!!!!!!!");
    }
    if (this->_state == State::Chipsets)
        throw nts::Exception("No links in the circuit!!!");
    return circuit;
}
