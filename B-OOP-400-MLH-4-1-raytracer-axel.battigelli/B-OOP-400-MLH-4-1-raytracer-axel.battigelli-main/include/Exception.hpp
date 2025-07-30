#pragma once

#include <source_location>

#include <exception>
#include <ostream>
#include <stdexcept>
#include <string>

namespace Raytracer
{
    /*
    ** Exception
    **
    ** Class for global exceptions errors
    */

    class Exception : public std::exception {
      private:
        std::runtime_error _what;
        const std::source_location _location;

        friend std::ostream &operator<<(std::ostream &stream, const Exception &self);

      public:
        Exception(const char *what,
            const std::source_location location = std::source_location::current())
            : _what(what), _location(location) {};
        Exception(const std::string &what,
            const std::source_location location = std::source_location::current())
            : _what(what), _location(location) {};
        [[nodiscard]] const char *what() const noexcept override { return this->_what.what(); }
    };

    inline std::ostream &operator<<(std::ostream &stream, const Exception &self)
    {
        stream << self._location.file_name() << ":" << self._location.line() << ":"
               << self._location.column() << ": " << self.what();
        return stream;
    }

    /*
    ** Exception
    **
    ** Class for global exceptions errors
    */

    class ParserException : public std::exception {
      private:
        std::runtime_error _what;
        const std::source_location _location;

        friend std::ostream &operator<<(std::ostream &stream, const ParserException &self);

      public:
        ParserException(const char *what,
            const std::source_location location = std::source_location::current())
            : _what(what), _location(location) {};
        ParserException(const std::string &what,
            const std::source_location location = std::source_location::current())
            : _what(what), _location(location) {};
        [[nodiscard]] const char *what() const noexcept override { return this->_what.what(); }
    };

    inline std::ostream &operator<<(std::ostream &stream, const ParserException &self)
    {
        stream << self._location.file_name() << ":" << self._location.line() << ":"
               << self._location.column() << ": " << self.what() << " --> generate Warnnig";
        return stream;
    }

} // namespace Raytracer
