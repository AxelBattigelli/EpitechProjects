#include "Component/ClockComponent.hpp"
#include "Exception.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <format>

nts::Tristate nts::component::ClockComponent::compute(std::size_t pin)
{
    if (pin != 1)
        throw nts::Exception(std::format("Bad pin {}", pin));
    return _value;
}

void nts::component::ClockComponent::simulate(std::size_t tick)
{
    if (tick == 0)
        return;
    switch (this->_value) {
        case Tristate::True:
            this->_value = Tristate::False;
            break;
        case Tristate::False:
            this->_value = Tristate::True;
            break;
        case Tristate::Undefined:
            this->_value = Tristate::Undefined;
            break;
    }
    if (this->_hasNextValue) {
        this->_value = this->_nextValue;
        this->_hasNextValue = false;
    }
}

void nts::component::ClockComponent::setValue(Tristate value)
{
    this->_hasNextValue = true;
    this->_nextValue = value;
}
