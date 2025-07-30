#include "Component/Gate4008.hpp"
#include "Exception.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <format>
#include <utility>

std::pair<nts::Tristate, nts::Tristate> nts::component::Gate4008::doTruthTable(
    nts::Tristate a, nts::Tristate b, nts::Tristate c)
{
    if (a == Undefined || b == Undefined || c == Undefined)
        return std::make_pair(Undefined, Undefined);
    if (a == False && b == False && c == False)
        return std::make_pair(False, False);
    if (a == True && b == False && c == False)
        return std::make_pair(False, True);
    if (a == False && b == True && c == False)
        return std::make_pair(False, True);
    if (a == True && b == True && c == False)
        return std::make_pair(True, False);
    if (a == False && b == False && c == True)
        return std::make_pair(False, True);
    if (a == True && b == False && c == True)
        return std::make_pair(True, False);
    if (a == False && b == True && c == True)
        return std::make_pair(True, False);
    if (a == True && b == True && c == True)
        return std::make_pair(True, True);
    throw nts::Exception("WTF???");
}

nts::Tristate nts::component::Gate4008::compute(std::size_t pin)
{
    if (pin == 8 || pin == 16)
        return Undefined;
    if (pin <= 9)
        return Tristate::Undefined;
    if (pin > 16 || pin == 0)
        throw nts::Exception(std::format("Bad pin {}", pin));
    if (pin == 10)
        return nts::component::Gate4008::doTruthTable(
            this->getLink(7), this->getLink(6), this->getLink(9))
            .second;
    if (pin == 11)
        return nts::component::Gate4008::doTruthTable(this->getLink(5), this->getLink(4),
            nts::component::Gate4008::doTruthTable(
                this->getLink(7), this->getLink(6), this->getLink(9))
                .first)
            .second;
    if (pin == 12)
        return nts::component::Gate4008::doTruthTable(this->getLink(3), this->getLink(2),
            nts::component::Gate4008::doTruthTable(this->getLink(5), this->getLink(4),
                nts::component::Gate4008::doTruthTable(
                    this->getLink(7), this->getLink(6), this->getLink(9))
                    .first)
                .first)
            .second;
    if (pin == 13)
        return nts::component::Gate4008::doTruthTable(this->getLink(1), this->getLink(15),
            nts::component::Gate4008::doTruthTable(this->getLink(3), this->getLink(2),
                nts::component::Gate4008::doTruthTable(this->getLink(5), this->getLink(4),
                    nts::component::Gate4008::doTruthTable(
                        this->getLink(7), this->getLink(6), this->getLink(9))
                        .first)
                    .first)
                .first)
            .second;
    if (pin == 14)
        return nts::component::Gate4008::doTruthTable(this->getLink(1), this->getLink(15),
            nts::component::Gate4008::doTruthTable(this->getLink(3), this->getLink(2),
                nts::component::Gate4008::doTruthTable(this->getLink(5), this->getLink(4),
                    nts::component::Gate4008::doTruthTable(
                        this->getLink(7), this->getLink(6), this->getLink(9))
                        .first)
                    .first)
                .first)
            .first;
    throw nts::Exception(std::format("Bad pin {}", pin));
}
