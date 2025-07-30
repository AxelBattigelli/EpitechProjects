#include "Component/Gate4017.hpp"
#include "Exception.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <format>
#include <iostream>

void nts::component::Gate4017::simulate(std::size_t /*tick*/)
{
    auto clock1 = this->getLink(14);
    auto clock2 = this->getLink(13);
    auto reset = this->getLink(15);

    if (this->_old_clock1 == Tristate::Undefined) {
        this->_old_clock1 = clock1;
    }
    if (this->_old_clock2 == Tristate::Undefined) {
        this->_old_clock2 = clock2;
    }
    if ((this->_old_clock1 == Tristate::False && clock1 == Tristate::True)
        || (this->_old_clock2 == Tristate::True && clock2 == Tristate::False)) {
        this->_data = (this->_data + 1) % 10;
    }
    if (reset != Tristate::False)
        this->_data = 0;
    this->_old_clock1 = clock1;
    this->_old_clock2 = clock2;
}

nts::Tristate nts::component::Gate4017::compute(std::size_t pin)
{
    if (pin > 14 || pin == 0)
        throw nts::Exception(std::format("Bad pin {}", pin));
    if (pin == 8 || pin == 14)
        return Tristate::Undefined;

    switch (pin) {
        case 3:
            return (this->_data == 0) ? Tristate::True : Tristate::False;
        case 2:
            return (this->_data == 1) ? Tristate::True : Tristate::False;
        case 4:
            return (this->_data == 2) ? Tristate::True : Tristate::False;
        case 7:
            return (this->_data == 3) ? Tristate::True : Tristate::False;
        case 10:
            return (this->_data == 4) ? Tristate::True : Tristate::False;
        case 1:
            return (this->_data == 5) ? Tristate::True : Tristate::False;
        case 5:
            return (this->_data == 6) ? Tristate::True : Tristate::False;
        case 6:
            return (this->_data == 7) ? Tristate::True : Tristate::False;
        case 9:
            return (this->_data == 8) ? Tristate::True : Tristate::False;
        case 11:
            return (this->_data == 9) ? Tristate::True : Tristate::False;
        case 12:
            return (this->_data < 5) ? Tristate::True : Tristate::False;
    }
    return Tristate::Undefined;
}
