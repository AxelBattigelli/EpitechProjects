#pragma once
#include <cstddef>
#include <iostream>

namespace nts
{
    enum Tristate {
        Undefined = (-1),
        False = static_cast<int>(false),
        True = static_cast<int>(true),
    };

    class IComponent {
      public:
        virtual ~IComponent() = default;
        virtual void simulate(std::size_t tick) = 0;
        virtual nts::Tristate compute(std::size_t pin) = 0;
        virtual void setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin) = 0;
    };

    inline std::ostream &operator<<(std::ostream &stream, const Tristate &self)
    {
        switch (self) {
            using enum Tristate;
            case Undefined:
                stream << "U";
                break;
            case False:
                stream << "0";
                break;
            case True:
                stream << "1";
                break;
        }
        return stream;
    }
} // namespace nts
