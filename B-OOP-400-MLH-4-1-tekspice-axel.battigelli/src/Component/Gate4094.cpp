#include "Component/Gate4094.hpp"
#include "Exception.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <format>

void nts::component::Gate4094::simulate(std::size_t /*tick*/)
{
    auto clock = this->getLink(3);
    auto strobe = this->getLink(1);
    auto outputEnable = this->getLink(15);
    if (this->_old_clock == Tristate::Undefined) {
        this->_old_clock = this->getLink(3);
    }
    if (this->_old_clock == Tristate::False && clock == Tristate::True) {
        if (strobe == Tristate::True && outputEnable == Tristate::True) {
            this->_data =
                (this->_data << 1) | static_cast<int>(this->getLink(2) == Tristate::True);
        }
        this->_qs = ((this->_data & 0b0100'0000) != 0) ? Tristate::True : Tristate::False;
    }
    if (this->_old_clock == Tristate::True && clock == Tristate::False) {
        this->_qps = ((this->_data & 0b0100'0000) != 0) ? Tristate::True : Tristate::False;
    }
    this->_old_clock = clock;
}

nts::Tristate nts::component::Gate4094::compute(std::size_t pin)
{
    auto outputEnable = this->getLink(15);
    Tristate value;
    switch (pin) {
        case 1:
        case 2:
        case 3:
        case 15:
        case 8:
        case 16:
            return Tristate::Undefined;
        case 4:
            value = ((this->_data & 0b0000'0001) != 0) ? Tristate::True : Tristate::False;
            break;
        case 5:
            value = ((this->_data & 0b0000'0010) != 0) ? Tristate::True : Tristate::False;
            break;
        case 6:
            value = ((this->_data & 0b0000'0100) != 0) ? Tristate::True : Tristate::False;
            break;
        case 7:
            value = ((this->_data & 0b0000'1000) != 0) ? Tristate::True : Tristate::False;
            break;
        case 14:
            value = ((this->_data & 0b0001'0000) != 0) ? Tristate::True : Tristate::False;
            break;
        case 13:
            value = ((this->_data & 0b0010'0000) != 0) ? Tristate::True : Tristate::False;
            break;
        case 12:
            value = ((this->_data & 0b0100'0000) != 0) ? Tristate::True : Tristate::False;
            break;
        case 11:
            value = ((this->_data & 0b1000'0000) != 0) ? Tristate::True : Tristate::False;
            break;
        case 9:
            return this->_qs;
        case 10:
            return this->_qps;
        default:
            throw nts::Exception(std::format("Bad pin {}", pin));
    }
    return ((outputEnable) != 0) ? value : Tristate::Undefined;
}
