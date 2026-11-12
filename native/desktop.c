#include <lean/lean.h>
#include <raylib.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>

// Cocoa requires the process main thread. Lean normally starts main on a worker.
__attribute__((constructor)) static void use_process_main_thread(void) {
    setenv("LEAN_MAIN_USE_THREAD", "0", 1);
}

static Sound break_sound;
static Sound place_sound;
static Sound craft_sound;

static Sound tone(double frequency, double seconds, double glide) {
    enum { rate = 22050, capacity = 4096 };
    int16_t samples[capacity];
    unsigned int count = (unsigned int)(rate * seconds);
    for (unsigned int i = 0; i < count; ++i) {
        double t = (double)i / rate;
        double fade = 1.0 - t / seconds;
        double phase = 6.283185307179586 * (frequency * t + 0.5 * glide * t * t);
        samples[i] = (int16_t)(3800.0 * fade * sin(phase));
    }
    Wave wave = { .frameCount = count, .sampleRate = rate, .sampleSize = 16,
                  .channels = 1, .data = samples };
    return LoadSoundFromWave(wave);
}

lean_obj_res terria_open(uint32_t width, uint32_t height, b_lean_obj_arg title) {
    SetTraceLogLevel(LOG_WARNING);
    InitWindow((int)width, (int)height, lean_string_cstr(title));
    if (!IsWindowReady()) {
        return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string("Could not open the raylib window.")));
    }
    SetTargetFPS(60);
    InitAudioDevice();
    if (IsAudioDeviceReady()) {
        break_sound = tone(154.0, 0.09, -620.0);
        place_sound = tone(220.0, 0.08, -920.0);
        craft_sound = tone(410.0, 0.17, 1800.0);
    }
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_close(void) {
    if (IsAudioDeviceReady()) {
        UnloadSound(break_sound);
        UnloadSound(place_sound);
        UnloadSound(craft_sound);
        CloseAudioDevice();
    }
    CloseWindow();
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_should_close(void) {
    return lean_io_result_mk_ok(lean_box(WindowShouldClose()));
}

lean_obj_res terria_frame_time(void) {
    return lean_io_result_mk_ok(lean_box_float((double)GetFrameTime()));
}

lean_obj_res terria_input(void) {
    uint32_t bits = 0;
    if (IsKeyDown(KEY_A) || IsKeyDown(KEY_LEFT)) bits |= 1u << 0;
    if (IsKeyDown(KEY_D) || IsKeyDown(KEY_RIGHT)) bits |= 1u << 1;
    if (IsKeyPressed(KEY_SPACE)) bits |= 1u << 2;
    if (IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) bits |= 1u << 3;
    if (IsMouseButtonPressed(MOUSE_BUTTON_RIGHT)) bits |= 1u << 4;
    if (IsKeyPressed(KEY_E)) bits |= 1u << 5;
    if (IsKeyPressed(KEY_S) || IsKeyPressed(KEY_F5)) bits |= 1u << 6;
    if (IsKeyPressed(KEY_L) || IsKeyPressed(KEY_F9)) bits |= 1u << 7;
    for (int index = 0; index < 6; ++index) {
        if (IsKeyPressed(KEY_ONE + index)) bits |= 1u << (8 + index);
    }
    return lean_io_result_mk_ok(lean_box_uint32(bits));
}

lean_obj_res terria_mouse_x(void) {
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)GetMouseX()));
}

lean_obj_res terria_mouse_y(void) {
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)GetMouseY()));
}

lean_obj_res terria_begin(uint32_t color) {
    BeginDrawing();
    ClearBackground(GetColor(color));
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_end(void) {
    EndDrawing();
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_rect(uint32_t x, uint32_t y, uint32_t width, uint32_t height, uint32_t color) {
    DrawRectangle((int32_t)x, (int32_t)y, (int)width, (int)height, GetColor(color));
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_gradient(uint32_t width, uint32_t height, uint32_t top, uint32_t bottom) {
    DrawRectangleGradientV(0, 0, (int)width, (int)height, GetColor(top), GetColor(bottom));
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_text(b_lean_obj_arg text, uint32_t x, uint32_t y, uint32_t size, uint32_t color) {
    DrawText(lean_string_cstr(text), (int32_t)x, (int32_t)y, (int)size, GetColor(color));
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_sound(uint32_t kind) {
    if (IsAudioDeviceReady()) {
        Sound sound = kind == 0 ? break_sound : kind == 1 ? place_sound : craft_sound;
        if (IsSoundValid(sound)) PlaySound(sound);
    }
    return lean_io_result_mk_ok(lean_box(0));
}
