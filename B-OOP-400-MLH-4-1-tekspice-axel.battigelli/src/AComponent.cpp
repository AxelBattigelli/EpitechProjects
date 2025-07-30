#include "AComponent.hpp"
#include "Exception.hpp"
#include "IComponent.hpp"
#include <algorithm>
#include <cstddef>
#include <ranges>
#include <utility>

nts::Tristate nts::AComponent::getLink(std::size_t pin)
{
    if (!this->_states.contains(pin))
        this->_states.insert_or_assign(pin, std::make_pair(Undefined, false));
    if (!this->_links.contains(pin))
        throw Exception("Unlinked Pin");
    if (this->_states.at(pin).second)
        return this->_states.at(pin).first;
    this->_states.at(pin).second = true;
    bool isUndecided = false;
    nts::Tristate state = Tristate::Undefined;
    for (const auto &link : this->_links.at(pin)) {
        const nts::Tristate newState = link.first->compute(link.second);
        if (newState == state || state == Tristate::Undefined) {
            state = newState;
            continue;
        }
        if (newState == Tristate::Undefined)
            continue;
        isUndecided = true;
    }
    if (isUndecided)
        state = Tristate::Undefined;
    this->_states.at(pin).first = state;
    this->_states.at(pin).second = false;
    return this->_states.at(pin).first;
}

void nts::AComponent::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin)
{
    if (static_cast<void *>((&other)) == static_cast<void *>(static_cast<nts::IComponent *>(this))
        && otherPin == pin)
        return;
    if (this->_links.contains(pin)
        && std::ranges::find(this->_links.at(pin), std::make_pair(&other, otherPin))
            != std::ranges::cend(this->_links.at(pin)))
        return;
    this->_links.emplace(pin, 0);
    this->_links.at(pin).emplace_back(&other, otherPin);
    other.setLink(otherPin, *this, pin);
    for (auto &link :
        std::ranges::take_view(this->_links.at(pin), this->_links.at(pin).size() - 1)) {
        link.first->setLink(link.second, other, otherPin);
    }
};
