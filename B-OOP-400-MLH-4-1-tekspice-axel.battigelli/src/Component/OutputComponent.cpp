#include "Component/OutputComponent.hpp"
#include "Exception.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <format>

nts::Tristate nts::component::OutputComponent::compute(std::size_t pin)
{
    if (pin == 1)
        return this->getLink(1);
    throw nts::Exception(std::format("Bad pin {}", pin));
}
