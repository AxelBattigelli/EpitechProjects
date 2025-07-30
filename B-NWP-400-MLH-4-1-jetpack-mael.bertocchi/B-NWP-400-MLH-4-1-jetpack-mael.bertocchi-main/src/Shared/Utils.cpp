/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Utils.cpp
*/

#include "Shared/Exception.hpp"
#include "Shared/Utils.hpp"

#include <algorithm>
#include <iostream>
#include <sstream>
#include <iomanip>
#include <format>

std::vector<std::string> Jetpack::Shared::Utils::SplitStr(const std::string& str, char delimiter)
{
    std::vector<std::string> words {};
    std::stringstream stream(str);
    std::string word {};

    while (std::getline(stream, word, delimiter)) {
        words.push_back(word);
    }
    return words;
}

const std::string Jetpack::Shared::Utils::GetTextOption(char **begin, char **end, const std::string& option)
{
    char **itr = std::find(begin, end, option);

    if (itr != end && ++itr != end) {
        return *itr;
    }
    throw Jetpack::Shared::Exception(std::format("Missing argument for option '{}'.", option));
}

bool Jetpack::Shared::Utils::DoesOptionExist(char **begin, char **end, const std::string& option)
{
    return std::find(begin, end, option) != end;
}

const std::string Jetpack::Shared::Utils::SerializeEvent(const Jetpack::Shared::Data::Event& event)
{
    std::string result = "aa" + CharAsHex(event.sender) + CharAsHex(event.receiver) + CharAsHex(event.header) + StringAsHex(event.data);
    std::ostringstream stream {};
    std::uint8_t checksum = 0;

    for (char c : result) {
        checksum += c;
    }
    stream << std::hex << std::setw(4) << std::setfill('0') << static_cast<std::int32_t>(checksum);
    return result + stream.str();
}

const Jetpack::Shared::Data::Event Jetpack::Shared::Utils::DeserializeEvent(const std::string& packet)
{
    if (packet.size() < 12 || packet.substr(0, 2) != "aa") {
        throw Jetpack::Shared::Exception("Invalid packet format");
    }

    std::string senderHex = packet.substr(2, 2);
    std::string receiverHex = packet.substr(4, 2);
    std::string headerHex = packet.substr(6, 2);
    std::string dataHex = packet.substr(8, packet.size() - 12);
    std::string checksumHex = packet.substr(packet.size() - 4, 4);

    std::string packetBody = packet.substr(0, packet.size() - 4);
    std::uint8_t calculatedChecksum = 0;

    for (char c : packetBody) {
        calculatedChecksum += c;
    }

    std::ostringstream checksumStream {};
    checksumStream << std::hex << std::setw(4) << std::setfill('0') << static_cast<std::int32_t>(calculatedChecksum);

    if (checksumStream.str() != checksumHex) {
        throw Jetpack::Shared::Exception("Invalid packet checksum");
    }

    Shared::Data::Id sender = static_cast<Shared::Data::Id>(std::stoi(senderHex, nullptr, 16));
    Shared::Data::Id receiver = static_cast<Shared::Data::Id>(std::stoi(receiverHex, nullptr, 16));
    Shared::Data::Header header = static_cast<Shared::Data::Header>(std::stoi(headerHex, nullptr, 16));
    std::string data {};

    for (size_t i = 0; i < dataHex.length(); i += 2) {
        if (i + 1 < dataHex.length()) {
            std::string byteStr = dataHex.substr(i, 2);
            data.push_back(static_cast<char>(std::stoi(byteStr, nullptr, 16)));
        }
    }

    return { sender, receiver, header, data };
}

const std::string Jetpack::Shared::Utils::StringAsHex(const std::string& str)
{
    std::ostringstream oss {};

    for (std::uint8_t c : str) {
        oss << std::setw(2) << std::setfill('0') << std::hex << static_cast<std::int32_t>(c);
    }
    return oss.str();
}

const std::string Jetpack::Shared::Utils::CharAsHex(const std::uint8_t c)
{
    std::ostringstream oss {};

    oss << std::setw(2) << std::setfill('0') << std::hex << static_cast<std::int32_t>(c);
    return oss.str();
}
