#include "Component/Gate4040.hpp"
#include "Exception.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <format>

void nts::component::Gate4040::simulate(std::size_t /*tick*/)
{
    auto clock = this->getLink(10);
    auto reset = this->getLink(11);

    if (reset != Tristate::False)
        this->_data = 0;

    if (this->_old_clock == Tristate::Undefined) {
        this->_old_clock = clock;
    }
    if (this->_old_clock == Tristate::True && clock == Tristate::False)
        this->_data++;
    if (this->_data >= 1 << 12)
        this->_data -= 1 << 12;
    this->_old_clock = clock;
}

nts::Tristate nts::component::Gate4040::compute(std::size_t pin)
{
    if (pin > 16 || pin == 0)
        throw nts::Exception(std::format("Bad pin {}", pin));

    if (pin == 8 || pin == 10 || pin == 11 || pin == 16)
        return Tristate::Undefined;

    auto reset = this->getLink(11);
    if (reset != Tristate::False)
        return Tristate::False;

    switch (pin) {
        case 9:
            return ((this->_data & 0b0000'0000'0001) != 0) ? Tristate::True : Tristate::False;
        case 7:
            return ((this->_data & 0b0000'0000'0010) != 0) ? Tristate::True : Tristate::False;
        case 6:
            return ((this->_data & 0b0000'0000'0100) != 0) ? Tristate::True : Tristate::False;
        case 5:
            return ((this->_data & 0b0000'0000'1000) != 0) ? Tristate::True : Tristate::False;
        case 3:
            return ((this->_data & 0b0000'0001'0000) != 0) ? Tristate::True : Tristate::False;
        case 2:
            return ((this->_data & 0b0000'0010'0000) != 0) ? Tristate::True : Tristate::False;
        case 4:
            return ((this->_data & 0b0000'0100'0000) != 0) ? Tristate::True : Tristate::False;
        case 13:
            return ((this->_data & 0b0000'1000'0000) != 0) ? Tristate::True : Tristate::False;
        case 12:
            return ((this->_data & 0b0001'0000'0000) != 0) ? Tristate::True : Tristate::False;
        case 14:
            return ((this->_data & 0b0010'0000'0000) != 0) ? Tristate::True : Tristate::False;
        case 15:
            return ((this->_data & 0b0100'0000'0000) != 0) ? Tristate::True : Tristate::False;
        case 1:
            return ((this->_data & 0b1000'0000'0000) != 0) ? Tristate::True : Tristate::False;
    }
    throw nts::Exception("WTF?");
}
