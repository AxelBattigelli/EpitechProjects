#include "Component/LoggerComponent.hpp"
#include "Exception.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <format>
#include <fstream>
#include <iostream>

nts::Tristate nts::component::LoggerComponent::compute(std::size_t pin)
{
    if (pin == 0 || pin > 10)
        throw nts::Exception(std::format("Bad pin {}", pin));
    return Tristate::Undefined;
}

void nts::component::LoggerComponent::simulate(std::size_t /*unused*/)
{
    Tristate const clock = this->getLink(9);
    Tristate const inhibit = this->getLink(10);

    if (clock == True && this->_clock == False && inhibit == False) {
        std::ofstream file("./log.bin", std::ios::app | std::ios::binary);
        if (!file) {
            std::cerr << "Error: Unable to open log.bin" << '\n';
            return;
        }

        char value = 0;
        for (std::size_t i = 1; i <= 8; ++i) {
            if (this->getLink(i) == Tristate::True)
                value |= (1 << (i - 1));
            if (this->getLink(i) == Tristate::Undefined) {
                this->_clock = clock;
                return;
            }
        }

        file.write(&value, 1);
        file.close();
    }
    this->_clock = clock;
}
