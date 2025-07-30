#pragma once

#include "AComponent.hpp"
#include "IComponent.hpp"
#include <cstddef>

namespace nts::component
{
    class ClockComponent : public AComponent {
      private:
        Tristate _value = Undefined;
        Tristate _nextValue = True;
        bool _hasNextValue = false;

      public:
        Tristate compute(std::size_t pin) override;
        void simulate(std::size_t /*tick*/) override;
        void setValue(Tristate);
    };
} // namespace nts::component
