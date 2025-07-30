#pragma once

#ifdef __cplusplus
extern "C"
{
#endif

#include <stdint.h>

#define LIB_MAGIC_VALUE (0x30fe1ebfc985ef27UL)
#define LIB_API_VERSION (1)

    typedef struct {
        char *ptr;
        uint64_t len;
    } string_view_t;

    typedef struct {
        uint64_t magic;       // must always be 0x30fe1ebfc985ef27 (a random number chosen once)
        uint64_t api_version; // The library is only valid if the api version matches exactly
        char type[16];        // Either "GAME" or "FRONTEND" (zero-terminated strings)
        string_view_t name;   // The user-facing name of the library
        string_view_t id;     // The ID of the library, used to store scores for example
    } lib_info_t; // This struct must be available as `ARCADE_AACMMN_LIB_INFO` in each shared
                  // object

    typedef struct frontend_handle
        frontend_handle_t;                    // Opaque type for the frontend to keep state in
    typedef struct core_handle core_handle_t; // Opaque type for the core to keep state in
    typedef struct game_handle game_handle_t; // Opaque type for the game to keep state in

    typedef struct {
        uint8_t r, g, b;
    } color_t;

    typedef struct {
        uint64_t x, y;
    } position_t;

    typedef struct {
        string_view_t image_path;
        char characters[2];
        color_t front, back;
    } tile_t;

    /**
     * @enum Key
     * @brief An enum representing a key on the keyboard
     */
    enum key_e {
        KB_SPACE = 0,     /*!< Space key */
        KB_ENTER = 1,     /*!< Enter key */
        KB_ESCAPE = 2,    /*!< Escape key */
        KB_BACKSPACE = 3, /*!< Backspace key */
        KB_DELETE = 4,    /*!< Delete key */
        KB_TAB = 5,       /*!< Tab key */

        KB_UP = 6,    /*!< Up key */
        KB_DOWN = 7,  /*!< Down key */
        KB_LEFT = 8,  /*!< Left key */
        KB_RIGHT = 9, /*!< Right key */

        KB_UPPER_A = 'A', /*!< A key in uppercase */
        KB_LOWER_A = 'a', /*!< A key in lowercase */

        KB_UPPER_B = 'B', /*!< B key in uppercase */
        KB_LOWER_B = 'b', /*!< B key in lowercase */

        KB_UPPER_C = 'C', /*!< C key in uppercase */
        KB_LOWER_C = 'c', /*!< C key in lowercase */

        KB_UPPER_D = 'D', /*!< D key in uppercase */
        KB_LOWER_D = 'd', /*!< D key in lowercase */

        KB_UPPER_E = 'E', /*!< E key in uppercase */
        KB_LOWER_E = 'e', /*!< E key in lowercase */

        KB_UPPER_F = 'F', /*!< F key in uppercase */
        KB_LOWER_F = 'f', /*!< F key in lowercase */

        KB_UPPER_G = 'G', /*!< G key in uppercase */
        KB_LOWER_G = 'g', /*!< G key in lowercase */

        KB_UPPER_H = 'H', /*!< H key in uppercase */
        KB_LOWER_H = 'h', /*!< H key in lowercase */

        KB_UPPER_I = 'I', /*!< I key in uppercase */
        KB_LOWER_I = 'i', /*!< I key in lowercase */

        KB_UPPER_J = 'J', /*!< J key in uppercase */
        KB_LOWER_J = 'j', /*!< J key in lowercase */

        KB_UPPER_K = 'K', /*!< K key in uppercase */
        KB_LOWER_K = 'k', /*!< K key in lowercase */

        KB_UPPER_L = 'L', /*!< L key in uppercase */
        KB_LOWER_L = 'l', /*!< L key in lowercase */

        KB_UPPER_M = 'M', /*!< M key in uppercase */
        KB_LOWER_M = 'm', /*!< M key in lowercase */

        KB_UPPER_N = 'N', /*!< N key in uppercase */
        KB_LOWER_N = 'n', /*!< N key in lowercase */

        KB_UPPER_O = 'O', /*!< O key in uppercase */
        KB_LOWER_O = 'o', /*!< O key in lowercase */

        KB_UPPER_P = 'P', /*!< P key in uppercase */
        KB_LOWER_P = 'p', /*!< P key in lowercase */

        KB_UPPER_Q = 'Q', /*!< Q key in uppercase */
        KB_LOWER_Q = 'q', /*!< Q key in lowercase */

        KB_UPPER_R = 'R', /*!< R key in uppercase */
        KB_LOWER_R = 'r', /*!< R key in lowercase */

        KB_UPPER_S = 'S', /*!< S key in uppercase */
        KB_LOWER_S = 's', /*!< S key in lowercase */

        KB_UPPER_T = 'T', /*!< T key in uppercase */
        KB_LOWER_T = 't', /*!< T key in lowercase */

        KB_UPPER_U = 'U', /*!< U key in uppercase */
        KB_LOWER_U = 'u', /*!< U key in lowercase */

        KB_UPPER_V = 'V', /*!< V key in uppercase */
        KB_LOWER_V = 'v', /*!< V key in lowercase */

        KB_UPPER_W = 'W', /*!< W key in uppercase */
        KB_LOWER_W = 'w', /*!< W key in lowercase */

        KB_UPPER_X = 'X', /*!< X key in uppercase */
        KB_LOWER_X = 'x', /*!< X key in lowercase */

        KB_UPPER_Y = 'Y', /*!< Y key in uppercase */
        KB_LOWER_Y = 'y', /*!< Y key in lowercase */

        KB_UPPER_Z = 'Z', /*!< Z key in uppercase */
        KB_LOWER_Z = 'z', /*!< Z key in lowercase */

        KB_NUM_0 = '0', /*!< 0 numpad key */
        KB_NUM_1 = '1', /*!< 1 numpad key */
        KB_NUM_2 = '2', /*!< 2 numpad key */
        KB_NUM_3 = '3', /*!< 3 numpad key */
        KB_NUM_4 = '4', /*!< 4 numpad key */
        KB_NUM_5 = '5', /*!< 5 numpad key */
        KB_NUM_6 = '6', /*!< 6 numpad key */
        KB_NUM_7 = '7', /*!< 7 numpad key */
        KB_NUM_8 = '8', /*!< 8 numpad key */
        KB_NUM_9 = '9', /*!< 9 numpad key */

        KB_UNKNOWN = -1 /*!< Unknown key */
    };

    enum mouse_button_e {
        MOUSE_LEFT,
        MOUSE_RIGHT,
        MOUSE_CENTER,

        MOUSE_NUM, // Must always be kept last
    };

    enum event_type_t {
        EVT_OTHER,
        EVT_KB,
        EVT_MOUSE,

        EVT_NUM, // Must always be kept last
    };

    typedef struct {
        event_type_t type;
    } event_other_t;

    typedef struct {
        event_type_t type;
        key_e key;
    } event_keyboard_t;

    typedef struct {
        event_type_t type;
        position_t position;
        mouse_button_e button;
    } event_mouse_t;

    typedef union {
        event_type_t type;
        event_keyboard_t keyboard;
        event_mouse_t mouse;
        event_other_t other;
    } event_t;

    frontend_handle_t *frontend_getInstance(void);

    /// Deinit the frontend instance
    ///
    /// The handle should not be used after this function.
    void frontend_destroyInstance(frontend_handle_t *);

    /// Init the frontend.
    ///
    /// It must be possible to call this function multiple times safely,
    /// although it is valid for it to only succeed once.
    void frontend_init(frontend_handle_t *);
    /// Deinit the frontend
    void frontend_deinit(frontend_handle_t *);

    void frontend_renderMap(
        frontend_handle_t *, tile_t **data, uint64_t length, const uint64_t *lengths);

    event_t frontend_getEvent(frontend_handle_t *);

    /// Init the game.
    ///
    /// It must be possible to successfully call this function multiple times,
    /// but not necesserarily have multiple instances running at the same time
    game_handle_t *game_getInstance(void);

    /// Deinit the game
    ///
    /// The handle should not be used after this function.
    void game_destroyInstance(game_handle_t *);

    tile_t **game_getMap(game_handle_t *, uint64_t *length, uint64_t **lengths);

    void game_updateMap(game_handle_t *, event_t);

    uint64_t game_getScore(game_handle_t *);

    void game_reset(game_handle_t *);

#ifdef __cplusplus
}
#endif
