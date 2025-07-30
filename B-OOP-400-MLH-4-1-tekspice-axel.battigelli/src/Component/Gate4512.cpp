#include "Component/Gate4512.hpp"
#include "Exception.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <cstdint>
#include <format>

nts::Tristate nts::component::Gate4512::compute(std::size_t pin)
{
    auto a = this->getLink(11);
    auto b = this->getLink(12);
    auto c = this->getLink(13);
    auto inhibit = this->getLink(10);
    auto oe = this->getLink(15);

    if (pin > 16 || pin == 0)
        throw nts::Exception(std::format("Bad pin {}", pin));
    if (pin != 14)
        return nts::Tristate::Undefined;
    if (oe != Tristate::False)
        return nts::Tristate::Undefined;
    if (inhibit != Tristate::False)
        return nts::Tristate::False;
    std::uint8_t address = ((a == Tristate::True) ? 0b001 : 0)
        | ((b == Tristate::True) ? 0b010 : 0) | ((c == Tristate::True) ? 0b100 : 0);
    if (address == 7)
        address++;
    address++;
    return this->getLink(address);
}
