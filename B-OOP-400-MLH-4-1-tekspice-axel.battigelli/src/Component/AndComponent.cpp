#include "Component/AndComponent.hpp"
#include "Exception.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <format>

nts::Tristate nts::component::AndComponent::compute(std::size_t pin)
{
    if (pin == 1)
        return Tristate::Undefined;
    if (pin == 2)
        return Tristate::Undefined;
    if (pin != 3 || pin == 0)
        throw nts::Exception(std::format("Bad pin {}", pin));
    Tristate const left = this->getLink(1);
    Tristate const right = this->getLink(2);
    if (left == False && right == False)
        return False;
    if (left == False && right == True)
        return False;
    if (left == False && right == Undefined)
        return False;
    if (left == True && right == False)
        return False;
    if (left == True && right == True)
        return True;
    if (left == True && right == Undefined)
        return Undefined;
    if (left == Undefined && right == False)
        return False;
    if (left == Undefined && right == True)
        return Undefined;
    if (left == Undefined && right == Undefined)
        return Undefined;
    throw nts::Exception("WTF?");
}
