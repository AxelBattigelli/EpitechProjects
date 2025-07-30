#include "Component/Gate4011.hpp"
#include "Exception.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <format>

nts::component::Gate4011::Gate4011()
{
    this->_andComponents[0].setLink(1, *this, 1);
    this->_andComponents[0].setLink(2, *this, 2);
    this->_andComponents[1].setLink(1, *this, 5);
    this->_andComponents[1].setLink(2, *this, 6);
    this->_andComponents[2].setLink(1, *this, 8);
    this->_andComponents[2].setLink(2, *this, 9);
    this->_andComponents[3].setLink(1, *this, 12);
    this->_andComponents[3].setLink(2, *this, 13);

    this->_notComponents[0].setLink(1, this->_andComponents[0], 3);
    this->_notComponents[0].setLink(2, *this, 3);
    this->_notComponents[1].setLink(1, this->_andComponents[1], 3);
    this->_notComponents[1].setLink(2, *this, 4);
    this->_notComponents[2].setLink(1, this->_andComponents[2], 3);
    this->_notComponents[2].setLink(2, *this, 10);
    this->_notComponents[3].setLink(1, this->_andComponents[3], 3);
    this->_notComponents[3].setLink(2, *this, 11);
}

nts::Tristate nts::component::Gate4011::compute(std::size_t pin)
{
    if (pin == 7 || pin == 14)
        return Tristate::Undefined;
    if (pin > 14 || pin == 0)
        throw nts::Exception(std::format("Bad pin {}", pin));
    return Tristate::Undefined;
}
