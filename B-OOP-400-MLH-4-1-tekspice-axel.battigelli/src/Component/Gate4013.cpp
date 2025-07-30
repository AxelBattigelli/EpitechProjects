#include "Component/Gate4013.hpp"
#include "Exception.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <format>

void nts::component::Gate4013::simulatePart(
    Tristate &oldClock, Tristate clock, Tristate reset, Tristate data, Tristate set, Tristate &q)
{
    if (oldClock == Tristate::Undefined) {
        oldClock = clock;
    }
    if (reset == Tristate::True) {
        q = Tristate::False;
        oldClock = clock;
        return;
    }
    if (set == Tristate::True) {
        q = Tristate::True;
        oldClock = clock;
        return;
    }
    if (oldClock == Tristate::False && clock == True)
        q = data;
    oldClock = clock;
}

void nts::component::Gate4013::simulate(std::size_t /*tick*/)
{
    auto clock1 = this->getLink(3);
    auto reset1 = this->getLink(4);
    auto data1 = this->getLink(5);
    auto set1 = this->getLink(6);
    auto clock2 = this->getLink(11);
    auto reset2 = this->getLink(10);
    auto data2 = this->getLink(9);
    auto set2 = this->getLink(8);

    nts::component::Gate4013::simulatePart(
        this->_old_clock1, clock1, reset1, data1, set1, this->_q1);
    nts::component::Gate4013::simulatePart(
        this->_old_clock2, clock2, reset2, data2, set2, this->_q2);
}

nts::Tristate nts::component::Gate4013::compute(std::size_t pin)
{
    if (pin > 14 || pin == 0)
        throw nts::Exception(std::format("Bad pin {}", pin));

    if (pin == 1 && this->getLink(4) == Tristate::True && this->getLink(6) == Tristate::True)
        return Tristate::True;
    if (pin == 2 && this->getLink(4) == Tristate::True && this->getLink(6) == Tristate::True)
        return Tristate::True;

    if (pin == 13 && this->getLink(10) == Tristate::True && this->getLink(8) == Tristate::True)
        return Tristate::True;
    if (pin == 12 && this->getLink(10) == Tristate::True && this->getLink(8) == Tristate::True)
        return Tristate::True;

    if (pin == 1)
        return this->_q1;
    if (pin == 13)
        return this->_q2;

    if (pin == 2)
        return (this->_q1 != 0) ? Tristate::False : Tristate::True;
    if (pin == 12)
        return (this->_q2 != 0) ? Tristate::False : Tristate::True;

    return Tristate::Undefined;
}
