#include "Circuit.hpp"
#include "CircuitParser.hpp"
#include "Component/ClockComponent.hpp"
#include "Component/InputComponent.hpp"
#include "Component/OutputComponent.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <iostream>
#include <istream>
#include <iterator>
#include <memory>
#include <string>
#include <utility>

nts::Circuit::Circuit(std::istream &stream)
{
    *this = CircuitParser().parse(stream);
}

void nts::Circuit::addComponent(const std::string &name, std::unique_ptr<IComponent> element)
{
    if (dynamic_cast<nts::component::InputComponent *>(element.get()) != nullptr) {
        this->_inputs.insert_or_assign(name, element.get());
        this->_displays.insert_or_assign(name, element.get());
    }
    if (dynamic_cast<nts::component::OutputComponent *>(element.get()) != nullptr)
        this->_displays.insert_or_assign(name, element.get());
    if (dynamic_cast<nts::component::ClockComponent *>(element.get()) != nullptr) {
        this->_inputs.insert_or_assign(name, element.get());
        this->_displays.insert_or_assign(name, element.get());
    }
    this->_components.push_back(std::move(element));
}

bool nts::Circuit::display() const
{
    std::cout << "tick: " << this->_tick << "\n";
    std::cout << "input(s):\n";
    for (const auto &output : this->_displays) {
        if (dynamic_cast<component::OutputComponent *>(output.second) != nullptr)
            continue;
        std::string const key = output.first;
        Tristate const value = output.second->compute(1);
        std::cout << "  " << key << ": " << value << "\n";
    }
    std::cout << "output(s):\n";
    for (const auto &output : this->_displays) {
        if (dynamic_cast<component::OutputComponent *>(output.second) == nullptr)
            continue;
        std::string const key = output.first;
        Tristate const value = output.second->compute(1);
        std::cout << "  " << key << ": " << value << "\n";
    }
    return true;
}

bool nts::Circuit::changeInput(const std::string &name, nts::Tristate value)
{
    std::cerr << "Input '" << name << "' = " << value << "\n";
    if (!this->_inputs.contains(name))
        return false;
    if (dynamic_cast<nts::component::InputComponent *>(this->_inputs.at(name)) != nullptr) {
        dynamic_cast<nts::component::InputComponent *>(this->_inputs.at(name))->setValue(value);
        return true;
    }
    if (dynamic_cast<nts::component::ClockComponent *>(this->_inputs.at(name)) != nullptr) {
        dynamic_cast<nts::component::ClockComponent *>(this->_inputs.at(name))->setValue(value);
        return true;
    }
    return false;
}

void nts::Circuit::simulate(std::size_t tick)
{
    this->_tick = tick;
    for (auto &part : this->_components)
        part->simulate(1);
    for (auto &part : this->_components)
        part->simulate(0);
}

nts::Tristate nts::Circuit::compute(std::size_t pin) {
    auto it = this->_displays.cbegin();
    std::ranges::advance(it, pin - 1);
    return it->second->compute(1);
}
