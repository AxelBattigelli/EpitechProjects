#pragma once

#include "AComponent.hpp"
#include "IComponent.hpp"
#include <cstddef>

namespace nts::component
{
    class Gate4013 : public AComponent {
      private:
        Tristate _old_clock1 = Tristate::Undefined;
        Tristate _q1 = Tristate::Undefined;
        Tristate _old_clock2 = Tristate::Undefined;
        Tristate _q2 = Tristate::Undefined;
        static void simulatePart(Tristate &, Tristate, Tristate, Tristate, Tristate, Tristate &);

      public:
        void simulate(std::size_t tick) override;
        Tristate compute(std::size_t pin) override;
    };
} // namespace nts::component
