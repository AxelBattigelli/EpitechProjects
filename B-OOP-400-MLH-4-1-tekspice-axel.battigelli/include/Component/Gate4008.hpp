#pragma once

#include "AComponent.hpp"
#include "IComponent.hpp"
#include <cstddef>
#include <utility>

namespace nts::component
{
    class Gate4008 : public AComponent {
      private:
        static std::pair<Tristate, Tristate> doTruthTable(Tristate, Tristate, Tristate);

      public:
        Tristate compute(std::size_t pin) override;
    };
} // namespace nts::component
