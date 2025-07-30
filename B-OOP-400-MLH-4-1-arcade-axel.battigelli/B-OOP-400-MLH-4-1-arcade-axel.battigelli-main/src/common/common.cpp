#include "Shared/Data.hpp"
#include "ffi.hpp"

tile_t convertGrToFrTile(const Arcade::Shared::Data::Tile &in)
{
    return tile_t{
        .image_path =
            {
                .ptr = const_cast<char *>(in.asset.data()),
                .len = in.asset.size(),
            },
        .characters =
            {
                in.text[0],
                in.text[1],
            },
        .front =
            {
                .r = in.fg.r,
                .g = in.fg.g,
                .b = in.fg.b,
            },
        .back =
            {
                .r = in.bg.r,
                .g = in.bg.g,
                .b = in.bg.b,
            },
    };
}

Arcade::Shared::Data::Tile convertFrToGrTile(const tile_t &in)
{
    return {
        .text = {in.characters[0], in.characters[1]},
        .asset = std::string(in.image_path.ptr, in.image_path.len),
        .fg =
            {
                .r = in.front.r,
                .g = in.front.g,
                .b = in.front.b,
            },
        .bg =
            {
                .r = in.back.r,
                .g = in.back.g,
                .b = in.back.b,
            },
    };
}

static event_t convertGrToFrMouse(
    const std::pair<Arcade::Shared::Data::MouseButton, Arcade::Shared::Data::Vector2<std::int32_t>>
        &evt)
{
    if (evt.second.x < 0 || evt.second.y < 0)
        return {.other = {.type = EVT_OTHER}};
    return {.mouse = event_mouse_t{
                .type = EVT_MOUSE,
                .position =
                    {
                        .x = (uint64_t) evt.second.x,
                        .y = (uint64_t) evt.second.y,
                    },
                .button =
                    [](const Arcade::Shared::Data::MouseButton &btn) {
                        switch (btn) {
                            case Arcade::Shared::Data::MouseButton::MB_LEFT:
                                return MOUSE_LEFT;
                            case Arcade::Shared::Data::MouseButton::MB_CENTER:
                                return MOUSE_CENTER;
                            case Arcade::Shared::Data::MouseButton::MB_RIGHT:
                                return MOUSE_RIGHT;
                        }
                        throw "Unreacheable";
                    }(evt.first),
            }};
}

template<class... Ts> struct overloaded : Ts... {
    using Ts::operator()...;
};

event_t convertGrToFrEvent(const Arcade::Shared::Data::Event &in)
{
    event_t res;
    std::visit(
        overloaded{[&](const std::monostate &other) { res.other = event_other_t(EVT_OTHER); },
            [&](const std::pair<Arcade::Shared::Data::MouseButton,
                Arcade::Shared::Data::Vector2<std::int32_t>> &mouse) {
                res = convertGrToFrMouse(mouse);
            },
            [&](const Arcade::Shared::Data::Key &key) {
                res.keyboard = event_keyboard_t{.type = EVT_KB, .key = (key_e) key};
            }},
        in);
    return res;
}

static std::pair<Arcade::Shared::Data::MouseButton, Arcade::Shared::Data::Vector2<std::int32_t>> convertFrToGrMouse(
    const event_mouse_t &evt)
{
    return std::make_pair(
        [](mouse_button_e btn) {
            switch (btn) {
                case MOUSE_LEFT:
                    return Arcade::Shared::Data::MouseButton::MB_LEFT;
                case MOUSE_CENTER:
                    return Arcade::Shared::Data::MouseButton::MB_CENTER;
                case MOUSE_RIGHT:
                    return Arcade::Shared::Data::MouseButton::MB_RIGHT;
            }
            throw "Unreacheable";
        }(evt.button),
        Arcade::Shared::Data::Vector2<std::int32_t>{
            .x = (int) evt.position.x, .y = (int) evt.position.y});
}

Arcade::Shared::Data::Event convertFrToGrEvent(const event_t &in)
{
    switch (in.type) {
        case EVT_OTHER:
            return Arcade::Shared::Data::UNKNOWN_KB; // std::monostate();
        case EVT_KB:
            return (Arcade::Shared::Data::Key) in.keyboard.key;
        case EVT_MOUSE:
            return convertFrToGrMouse(in.mouse);
        default:
            return Arcade::Shared::Data::UNKNOWN_KB;
    }
}
