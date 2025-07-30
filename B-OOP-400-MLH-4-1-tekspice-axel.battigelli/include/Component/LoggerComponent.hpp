#pragma once

#include "AComponent.hpp"
#include "IComponent.hpp"
#include <cstddef>

namespace nts::component
{
    class LoggerComponent : public AComponent {
      private:
        Tristate _clock = Undefined;

      public:
        Tristate compute(std::size_t pin) override;
        void simulate(std::size_t tick) override;
    };
} // namespace nts::component
