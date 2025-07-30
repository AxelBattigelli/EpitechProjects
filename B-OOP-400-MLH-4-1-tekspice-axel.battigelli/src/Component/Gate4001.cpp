#include "Component/Gate4001.hpp"
#include "Exception.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <format>

nts::component::Gate4001::Gate4001()
{
    this->_orComponents[0].setLink(1, *this, 1);
    this->_orComponents[0].setLink(2, *this, 2);
    this->_orComponents[1].setLink(1, *this, 5);
    this->_orComponents[1].setLink(2, *this, 6);
    this->_orComponents[2].setLink(1, *this, 8);
    this->_orComponents[2].setLink(2, *this, 9);
    this->_orComponents[3].setLink(1, *this, 12);
    this->_orComponents[3].setLink(2, *this, 13);

    this->_notComponents[0].setLink(1, this->_orComponents[0], 3);
    this->_notComponents[0].setLink(2, *this, 3);
    this->_notComponents[1].setLink(1, this->_orComponents[1], 3);
    this->_notComponents[1].setLink(2, *this, 4);
    this->_notComponents[2].setLink(1, this->_orComponents[2], 3);
    this->_notComponents[2].setLink(2, *this, 10);
    this->_notComponents[3].setLink(1, this->_orComponents[3], 3);
    this->_notComponents[3].setLink(2, *this, 11);
}

nts::Tristate nts::component::Gate4001::compute(std::size_t pin)
{
    if (pin == 7 || pin == 14)
        return Undefined;
    if (pin > 14 || pin == 0)
        throw nts::Exception(std::format("Bad pin {}", pin));
    return Tristate::Undefined;
}
