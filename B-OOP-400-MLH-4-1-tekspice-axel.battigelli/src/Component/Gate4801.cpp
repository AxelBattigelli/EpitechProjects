#include "Component/Gate4801.hpp"
#include "Exception.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <cstdint>
#include <format>

void nts::component::Gate4801::simulate(std::size_t /*tick*/)
{
    if (this->getLink(18) != Tristate::False || this->getLink(21) != Tristate::False)
        return;

    int const result = (static_cast<int>(this->getLink(8) == Tristate::True) << 0)
        | (static_cast<int>(this->getLink(7) == Tristate::True) << 1)
        | (static_cast<int>(this->getLink(6) == Tristate::True) << 2)
        | (static_cast<int>(this->getLink(5) == Tristate::True) << 3)
        | (static_cast<int>(this->getLink(4) == Tristate::True) << 4)
        | (static_cast<int>(this->getLink(3) == Tristate::True) << 5)
        | (static_cast<int>(this->getLink(2) == Tristate::True) << 6)
        | (static_cast<int>(this->getLink(1) == Tristate::True) << 7)
        | (static_cast<int>(this->getLink(23) == Tristate::True) << 8)
        | (static_cast<int>(this->getLink(22) == Tristate::True) << 9);

    unsigned int const input = (this->getLink(9) == Tristate::True ? 0b0000'0001 : 0)
        | (this->getLink(10) == Tristate::True ? 0b0000'0010 : 0)
        | (this->getLink(11) == Tristate::True ? 0b0000'0100 : 0)
        | (this->getLink(13) == Tristate::True ? 0b0000'1000 : 0)
        | (this->getLink(14) == Tristate::True ? 0b0001'0000 : 0)
        | (this->getLink(15) == Tristate::True ? 0b0010'0000 : 0)
        | (this->getLink(16) == Tristate::True ? 0b0100'0000 : 0)
        | (this->getLink(17) == Tristate::True ? 0b1000'0000 : 0);

    this->_data.at(result) = input;
}

nts::Tristate nts::component::Gate4801::compute(std::size_t pin)
{
    if (pin > 24 || pin == 0)
        throw nts::Exception(std::format("Bad pin {}", pin));
    if (pin <= 8 || pin == 12 || pin >= 18)
        return Tristate::Undefined;
    if (this->getLink(18) != Tristate::False)
        return Tristate::Undefined;
    if (this->getLink(21) != Tristate::True)
        return Tristate::Undefined;
    if (this->getLink(20) != Tristate::False)
        return Tristate::Undefined;

    int const result = (static_cast<int>(this->getLink(8) == Tristate::True) << 0)
        | (static_cast<int>(this->getLink(7) == Tristate::True) << 1)
        | (static_cast<int>(this->getLink(6) == Tristate::True) << 2)
        | (static_cast<int>(this->getLink(5) == Tristate::True) << 3)
        | (static_cast<int>(this->getLink(4) == Tristate::True) << 4)
        | (static_cast<int>(this->getLink(3) == Tristate::True) << 5)
        | (static_cast<int>(this->getLink(2) == Tristate::True) << 6)
        | (static_cast<int>(this->getLink(1) == Tristate::True) << 7)
        | (static_cast<int>(this->getLink(23) == Tristate::True) << 8)
        | (static_cast<int>(this->getLink(22) == Tristate::True) << 9);
    uint8_t const byte = this->_data.at(result);

    switch (pin) {
        case 9:
            return ((byte & 0b0000'0001) != 0) ? Tristate::True : Tristate::False;
        case 10:
            return ((byte & 0b0000'0010) != 0) ? Tristate::True : Tristate::False;
        case 11:
            return ((byte & 0b0000'0100) != 0) ? Tristate::True : Tristate::False;
        case 13:
            return ((byte & 0b0000'1000) != 0) ? Tristate::True : Tristate::False;
        case 14:
            return ((byte & 0b0001'0000) != 0) ? Tristate::True : Tristate::False;
        case 15:
            return ((byte & 0b0010'0000) != 0) ? Tristate::True : Tristate::False;
        case 16:
            return ((byte & 0b0100'0000) != 0) ? Tristate::True : Tristate::False;
        case 17:
            return ((byte & 0b1000'0000) != 0) ? Tristate::True : Tristate::False;
    }
    return Tristate::Undefined;
}
