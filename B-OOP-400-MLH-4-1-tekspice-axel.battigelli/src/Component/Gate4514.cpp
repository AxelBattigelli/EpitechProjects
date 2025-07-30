#include "Component/Gate4514.hpp"
#include "Exception.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <format>

nts::Tristate nts::component::Gate4514::compute(std::size_t pin)
{
    auto inhibit = this->getLink(23);
    if (pin > 24 || pin == 0)
        throw nts::Exception(std::format("Bad pin {}", pin));
    if (pin <= 3 || pin == 12 || pin >= 21)
        return Tristate::Undefined;

    if (inhibit != Tristate::False)
        return Tristate::False;

    if (this->_value == 16)
        return Tristate::Undefined;
    switch (pin) {
        case 11:
            return (static_cast<int>(this->_value == 0) != 0) ? Tristate::True : Tristate::False;
        case 9:
            return (static_cast<int>(this->_value == 1) != 0) ? Tristate::True : Tristate::False;
        case 10:
            return (static_cast<int>(this->_value == 2) != 0) ? Tristate::True : Tristate::False;
        case 8:
            return (static_cast<int>(this->_value == 3) != 0) ? Tristate::True : Tristate::False;
        case 7:
            return (static_cast<int>(this->_value == 4) != 0) ? Tristate::True : Tristate::False;
        case 6:
            return (static_cast<int>(this->_value == 5) != 0) ? Tristate::True : Tristate::False;
        case 5:
            return (static_cast<int>(this->_value == 6) != 0) ? Tristate::True : Tristate::False;
        case 4:
            return (static_cast<int>(this->_value == 7) != 0) ? Tristate::True : Tristate::False;
        case 18:
            return (static_cast<int>(this->_value == 8) != 0) ? Tristate::True : Tristate::False;
        case 17:
            return (static_cast<int>(this->_value == 9) != 0) ? Tristate::True : Tristate::False;
        case 20:
            return (static_cast<int>(this->_value == 10) != 0) ? Tristate::True : Tristate::False;
        case 19:
            return (static_cast<int>(this->_value == 11) != 0) ? Tristate::True : Tristate::False;
        case 14:
            return (static_cast<int>(this->_value == 12) != 0) ? Tristate::True : Tristate::False;
        case 13:
            return (static_cast<int>(this->_value == 13) != 0) ? Tristate::True : Tristate::False;
        case 16:
            return (static_cast<int>(this->_value == 14) != 0) ? Tristate::True : Tristate::False;
        case 15:
            return (static_cast<int>(this->_value == 15) != 0) ? Tristate::True : Tristate::False;
        default:
            break;
    }
    throw nts::Exception("WTF?");
}

void nts::component::Gate4514::simulate(std::size_t /*unused*/)
{
    auto a = this->getLink(2);
    auto b = this->getLink(3);
    auto c = this->getLink(21);
    auto d = this->getLink(22);
    auto strobe = this->getLink(1);

    if (strobe != Tristate::True)
        return;

    if (a == Tristate::Undefined || b == Tristate::Undefined || c == Tristate::Undefined
        || d == Tristate::Undefined)
        this->_value = 16;
    else
        this->_value = ((a == Tristate::True) ? 0b0001 : 0) | ((b == Tristate::True) ? 0b0010 : 0)
            | ((c == Tristate::True) ? 0b0100 : 0) | ((d == Tristate::True) ? 0b1000 : 0);
}
