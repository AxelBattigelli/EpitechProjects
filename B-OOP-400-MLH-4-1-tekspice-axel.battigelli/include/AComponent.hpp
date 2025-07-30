
#pragma once
#include "IComponent.hpp"
#include <unordered_map>
#include <cstddef>
#include <utility>
#include <vector>

namespace nts
{
    class AComponent : public IComponent {
      private:
        std::unordered_map<std::size_t, std::vector<std::pair<IComponent *, std::size_t>>> _links;
        std::unordered_map<std::size_t, std::pair<Tristate, bool>> _states;

      protected:
        Tristate getLink(std::size_t pin);

      public:
        void simulate(std::size_t /*tick*/) override {};
        void setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin) final;
    };
} // namespace nts
