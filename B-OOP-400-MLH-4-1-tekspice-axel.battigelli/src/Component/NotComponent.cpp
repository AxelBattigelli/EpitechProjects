#include "Component/NotComponent.hpp"
#include "Exception.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <format>

nts::Tristate nts::component::NotComponent::compute(std::size_t pin)
{
    if (pin == 1)
        return Tristate::Undefined;
    if (pin != 2 || pin == 0)
        throw nts::Exception(std::format("Bad pin {}", pin));
    switch (this->getLink(1)) {
        case True:
            return Tristate::False;
        case False:
            return Tristate::True;
        case Undefined:
            return Tristate::Undefined;
    }
    throw nts::Exception("WTF?");
}
