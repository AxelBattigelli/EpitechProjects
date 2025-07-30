#include "Component/Gate4069.hpp"
#include "Exception.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <format>

nts::component::Gate4069::Gate4069()
{
    this->_notComponents[0].setLink(1, *this, 1);
    this->_notComponents[0].setLink(2, *this, 2);
    this->_notComponents[1].setLink(1, *this, 3);
    this->_notComponents[1].setLink(2, *this, 4);
    this->_notComponents[2].setLink(1, *this, 5);
    this->_notComponents[2].setLink(2, *this, 6);
    this->_notComponents[3].setLink(1, *this, 9);
    this->_notComponents[3].setLink(2, *this, 8);
    this->_notComponents[4].setLink(1, *this, 11);
    this->_notComponents[4].setLink(2, *this, 10);
    this->_notComponents[5].setLink(1, *this, 13);
    this->_notComponents[5].setLink(2, *this, 12);
}

nts::Tristate nts::component::Gate4069::compute(std::size_t pin)
{
    if (pin == 7 || pin == 14)
        return Undefined;
    if (pin > 14 || pin == 0)
        throw nts::Exception(std::format("Bad pin {}", pin));
    return Tristate::Undefined;
}
