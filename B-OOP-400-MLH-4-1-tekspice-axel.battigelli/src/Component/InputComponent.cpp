#include "Component/InputComponent.hpp"
#include "Exception.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <format>

nts::Tristate nts::component::InputComponent::compute(size_t pin)
{
    if (pin == 1)
        return this->_value;
    throw nts::Exception(std::format("Invalid pin {}", pin));
}

void nts::component::InputComponent::simulate(std::size_t tick)
{
    if (tick == 0)
        return;
    this->_value = this->_nextValue;
}

void nts::component::InputComponent::setValue(Tristate value)
{
    this->_nextValue = value;
}
