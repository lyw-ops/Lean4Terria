#include <lean/lean.h>
#include <raylib.h>
#include <rlgl.h>
#include <dlfcn.h>
#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

enum { asset_capacity = 2048, asset_name_capacity = 96, asset_byte_limit = 32 * 1024 * 1024 };
static char original_content[PATH_MAX];
static Texture2D original_cursor;
static bool original_cursor_layer_drawn;
static RenderTexture2D original_liquid_layers[2];
static Shader magic_missile_shader;
static bool magic_missile_shader_tried;
static Shader queen_slime_shader;
static bool queen_slime_shader_tried;
static int missile_veins_loc, missile_noise_loc, missile_clock_loc, missile_flame_loc;
typedef struct {
    Music stream;
    unsigned char *data;
    bool tried;
} BossMusic;
static BossMusic wall_music, twins_music, destroyer_music, prime_music, plantera_music, golem_music;
static BossMusic temple_music, dungeon_music, skeletron_music;
static struct {
    char name[asset_name_capacity];
    Texture2D texture;
} assets[asset_capacity];

struct archive;
struct archive_entry;

/* macOS ships libarchive with bsdtar, but its headers are not in the SDK. */
static struct {
    struct archive *(*make)(void);
    int (*cab)(struct archive *);
    int (*open)(struct archive *, const void *, size_t);
    int (*next)(struct archive *, struct archive_entry **);
    ssize_t (*read)(struct archive *, void *, size_t);
    int (*release)(struct archive *);
} cabinet;
static void *archive_library;

static bool open_cabinet_library(void) {
    archive_library = dlopen("/usr/lib/libarchive.2.dylib", RTLD_LOCAL | RTLD_LAZY);
    if (!archive_library) return false;
    cabinet.make = dlsym(archive_library, "archive_read_new");
    cabinet.cab = dlsym(archive_library, "archive_read_support_format_cab");
    cabinet.open = dlsym(archive_library, "archive_read_open_memory");
    cabinet.next = dlsym(archive_library, "archive_read_next_header");
    cabinet.read = dlsym(archive_library, "archive_read_data");
    cabinet.release = dlsym(archive_library, "archive_read_free");
    return cabinet.make && cabinet.cab && cabinet.open && cabinet.next && cabinet.read && cabinet.release;
}

static uint32_t read_le32(const unsigned char *p) {
    return (uint32_t)p[0] | ((uint32_t)p[1] << 8) | ((uint32_t)p[2] << 16) | ((uint32_t)p[3] << 24);
}

static unsigned read_be16(const unsigned char *p) {
    return ((unsigned)p[0] << 8) | p[1];
}

static unsigned read_le16(const unsigned char *p) {
    return p[0] | ((unsigned)p[1] << 8);
}

static void write_le16(unsigned char *p, unsigned value) {
    p[0] = (unsigned char)value;
    p[1] = (unsigned char)(value >> 8);
}

static void write_le32(unsigned char *p, uint32_t value) {
    write_le16(p, value);
    write_le16(p + 2, value >> 16);
}

/* Decode the Steam 1.4.5.8 wave-bank entries for Raylib's PCM stream. */
static Music load_boss_music(unsigned index, uint32_t expected_frames, uint32_t expected_bytes,
        unsigned char **wave_data) {
    enum { header_size = 90, pcm_header_size = 44 };
    if (!original_content[0]) return (Music){0};
    char path[PATH_MAX];
    int path_length = snprintf(path, sizeof(path), "%s/Wave Bank.xwb", original_content);
    if (path_length < 0 || (size_t)path_length >= sizeof(path)) return (Music){0};
    FILE *file = fopen(path, "rb");
    if (!file) return (Music){0};
    unsigned char header[52], entry[24];
    unsigned char *wav = NULL;
    if (fread(header, 1, sizeof(header), file) != sizeof(header) ||
            memcmp(header, "WBND", 4) || read_le32(header + 4) != 46 ||
            read_le32(header + 8) != 44) goto done;
    uint32_t metadata_offset = read_le32(header + 20);
    uint32_t metadata_length = read_le32(header + 24);
    if (metadata_length < (index + 1) * sizeof(entry) ||
            fseek(file, (long)metadata_offset + index * sizeof(entry), SEEK_SET) ||
            fread(entry, 1, sizeof(entry), file) != sizeof(entry)) goto done;
    uint32_t format = read_le32(entry + 4);
    uint32_t offset = read_le32(entry + 8);
    uint32_t bytes = read_le32(entry + 12);
    uint32_t wave_offset = read_le32(header + 44);
    uint32_t wave_length = read_le32(header + 48);
    if (format != 0x1815888a || read_le32(entry + 16) != 0 ||
            read_le32(entry + 20) != expected_frames || bytes != expected_bytes ||
            offset > wave_length || bytes > wave_length - offset ||
            fseek(file, (long)wave_offset + offset, SEEK_SET)) goto done;
    wav = calloc(1, header_size + bytes);
    if (!wav) goto done;
    if (fread(wav + header_size, 1, bytes, file) != bytes) goto done;
    memcpy(wav, "RIFF", 4);
    write_le32(wav + 4, header_size + bytes - 8);
    memcpy(wav + 8, "WAVEfmt ", 8);
    write_le32(wav + 16, 50);
    write_le16(wav + 20, 2); /* MS ADPCM */
    write_le16(wav + 22, 2);
    write_le32(wav + 24, 44100);
    write_le32(wav + 28, 44100 * 140 / 128);
    write_le16(wav + 32, 140);
    write_le16(wav + 34, 4);
    write_le16(wav + 36, 32);
    write_le16(wav + 38, 128);
    write_le16(wav + 40, 7);
    static const int16_t coefficients[] = {256, 0, 512, -256, 0, 0, 192, 64,
                                          240, 0, 460, -208, 392, -232};
    for (unsigned i = 0; i < sizeof(coefficients) / sizeof(coefficients[0]); ++i)
        write_le16(wav + 42 + 2 * i, (uint16_t)coefficients[i]);
    memcpy(wav + 70, "fact", 4);
    write_le32(wav + 74, 4);
    write_le32(wav + 78, expected_frames);
    memcpy(wav + 82, "data", 4);
    write_le32(wav + 86, bytes);

    /* The static wave loader decodes MS ADPCM. The streaming loader
       advances through an ADPCM wave without producing its PCM samples. */
    Wave decoded = LoadWaveFromMemory(".wav", wav, header_size + bytes);
    free(wav);
    wav = NULL;
    if (!IsWaveValid(decoded) || decoded.frameCount != expected_frames ||
            decoded.sampleRate != 44100 || decoded.sampleSize != 16 || decoded.channels != 2) {
        UnloadWave(decoded);
        goto done;
    }
    uint32_t pcm_bytes = decoded.frameCount * 4;
    wav = malloc(pcm_header_size + pcm_bytes);
    if (!wav) {
        UnloadWave(decoded);
        goto done;
    }
    memcpy(wav, "RIFF", 4);
    write_le32(wav + 4, pcm_header_size + pcm_bytes - 8);
    memcpy(wav + 8, "WAVEfmt ", 8);
    write_le32(wav + 16, 16);
    write_le16(wav + 20, 1); /* PCM */
    write_le16(wav + 22, 2);
    write_le32(wav + 24, 44100);
    write_le32(wav + 28, 44100 * 4);
    write_le16(wav + 32, 4);
    write_le16(wav + 34, 16);
    memcpy(wav + 36, "data", 4);
    write_le32(wav + 40, pcm_bytes);
    memcpy(wav + pcm_header_size, decoded.data, pcm_bytes);
    UnloadWave(decoded);

    Music music = LoadMusicStreamFromMemory(".wav", wav, pcm_header_size + pcm_bytes);
    if (IsMusicValid(music)) {
        *wave_data = wav;
        fclose(file);
        return music;
    }
done:
    free(wav);
    fclose(file);
    return (Music){0};
}

static void update_music(BossMusic *music, bool active, float volume,
        unsigned index, uint32_t frames, uint32_t bytes) {
    if (active && IsAudioDeviceReady() && !music->tried) {
        music->tried = true;
        music->stream = load_boss_music(index, frames, bytes, &music->data);
    }
    if (IsMusicValid(music->stream)) {
        if (active) {
            SetMusicVolume(music->stream, volume);
            if (!IsMusicStreamPlaying(music->stream)) PlayMusicStream(music->stream);
            UpdateMusicStream(music->stream);
        } else if (IsMusicStreamPlaying(music->stream)) StopMusicStream(music->stream);
    }
}

static void update_boss_music(BossMusic *music, bool active, unsigned index, uint32_t frames, uint32_t bytes) {
    update_music(music, active, 0.45f, index, frames, bytes);
}

lean_obj_res terria_dungeon_music(uint8_t dungeon_playing, float dungeon_gain,
        uint8_t skeletron_playing, float skeletron_gain) {
    /* Music_23 and Music_104 point at entries 22 and 103 in the desktop bank. */
    update_music(&dungeon_music, dungeon_playing, 0.45f * dungeon_gain, 22, 4441472, 4857860);
    update_music(&skeletron_music, skeletron_playing, 0.45f * skeletron_gain, 103, 5726976, 6263880);
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_wall_music(uint8_t active) {
    /* Music_12 (Boss 2) points at wave-bank entry 10. */
    update_boss_music(&wall_music, active, 10, 2576384, 2817920);
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_twins_music(uint8_t active) {
    /* Music_97 (The Twins) points at wave-bank entry 96. */
    update_boss_music(&twins_music, active, 96, 5723264, 6259820);
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_destroyer_music(uint8_t active) {
    /* Music_92 (The Destroyer) points at wave-bank entry 91. */
    update_boss_music(&destroyer_music, active, 91, 5489408, 6004040);
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_prime_music(uint8_t active) {
    /* Music_98 (Skeletron Prime) points at wave-bank entry 97. */
    update_boss_music(&prime_music, active, 97, 5118464, 5598320);
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_plantera_music(uint8_t active) {
    /* Music_24 (Plantera) points at wave-bank entry 23. */
    update_boss_music(&plantera_music, active, 23, 3386880, 3704400);
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_golem_music(uint8_t active) {
    /* Music_17 (Boss 4) points at wave-bank entry 16. */
    update_boss_music(&golem_music, active, 16, 3880832, 4244660);
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_temple_music(uint8_t active) {
    /* Music_26 (Lihzahrd) points at wave-bank entry 25. */
    update_boss_music(&temple_music, active, 25, 4293504, 4696020);
    return lean_io_result_mk_ok(lean_box(0));
}

static void unload_boss_music(BossMusic *music) {
    if (IsMusicValid(music->stream)) UnloadMusicStream(music->stream);
    free(music->data);
    *music = (BossMusic){0};
}

/* XNB v5 and CAB both carry 32 KiB LZX frames. Build a single-file cabinet
   in memory, preserving the XNB frames and using its 64 KiB dictionary. */
static unsigned char *decode_xnb_lzx(const unsigned char *data, size_t bytes, size_t expected,
        size_t target, size_t *decoded_bytes) {
    enum { cab_header = 36, cab_folder = 8, cab_entry = 22, frame_bytes = 32768 };
    const size_t data_start = cab_header + cab_folder + cab_entry;
    const size_t capacity = data_start + bytes + 8 * ((target + frame_bytes - 1) / frame_bytes);
    unsigned char *cab = calloc(1, capacity);
    if (!cab) return NULL;
    size_t offset = data_start;
    unsigned frames = 0;
    size_t expanded = 0;
    while (bytes >= 2 && expanded < target) {
        bool explicit_size = data[0] == 255;
        size_t header = explicit_size ? 5 : 2;
        if (bytes < header) break;
        unsigned plain = explicit_size ? read_be16(data + 1) : frame_bytes;
        unsigned packed = read_be16(data + header - 2);
        if (!plain || !packed || plain > frame_bytes || packed > bytes - header ||
                plain > expected - expanded || 8 + packed > capacity - offset || frames == UINT16_MAX) break;
        write_le16(cab + offset + 4, packed);
        write_le16(cab + offset + 6, plain);
        memcpy(cab + offset + 8, data + header, packed);
        offset += 8 + packed;
        expanded += plain;
        data += header + packed;
        bytes -= header + packed;
        ++frames;
    }
    unsigned char *result = NULL;
    if (expanded >= target) {
        memcpy(cab, "MSCF", 4);
        write_le32(cab + 8, (uint32_t)offset);
        write_le32(cab + 16, cab_header + cab_folder);
        cab[24] = 3;
        cab[25] = 1;
        write_le16(cab + 26, 1);
        write_le16(cab + 28, 1);
        write_le32(cab + cab_header, (uint32_t)data_start);
        write_le16(cab + cab_header + 4, frames);
        write_le16(cab + cab_header + 6, (16 << 8) | 3);
        write_le32(cab + cab_header + cab_folder, (uint32_t)expanded);
        memcpy(cab + cab_header + cab_folder + 16, "entry", 6);
        struct archive *reader = cabinet.make();
        struct archive_entry *entry = NULL;
        if (reader && cabinet.cab(reader) == 0 && cabinet.open(reader, cab, offset) == 0 &&
                cabinet.next(reader, &entry) == 0) {
            result = malloc(expanded);
            if (result && cabinet.read(reader, result, expanded) != (ssize_t)expanded) {
                free(result);
                result = NULL;
            }
        }
        if (reader) cabinet.release(reader);
    }
    free(cab);
    if (result) *decoded_bytes = expanded;
    return result;
}

static unsigned char *read_asset_file(const char *folder, const char *name, size_t *size) {
    char path[PATH_MAX];
    int length = snprintf(path, sizeof(path), "%s/%s/%s.xnb", original_content, folder, name);
    if (length < 0 || (size_t)length >= sizeof(path)) return NULL;
    FILE *file = fopen(path, "rb");
    if (!file) return NULL;
    unsigned char *data = NULL;
    if (fseek(file, 0, SEEK_END) == 0) {
        long bytes = ftell(file);
        if (bytes >= 10 && bytes <= asset_byte_limit && fseek(file, 0, SEEK_SET) == 0) {
            *size = (size_t)bytes;
            data = malloc(*size);
            if (data && fread(data, 1, *size, file) != *size) {
                free(data);
                data = NULL;
            }
        }
    }
    fclose(file);
    return data;
}

static unsigned read_xnb_int(const unsigned char **data, size_t *remaining) {
    unsigned value = 0;
    for (unsigned shift = 0; shift <= 28 && *remaining; shift += 7) {
        unsigned byte = *(*data)++;
        --*remaining;
        if (shift == 28 && byte > 15) return UINT_MAX;
        value |= (byte & 127) << shift;
        if (byte < 128) return value;
    }
    return UINT_MAX;
}

#include "terraria_font.h"
static TerrariaFont combat_text_font, combat_crit_font;

static bool xnb_texture_header(const unsigned char **data, size_t *bytes) {
    static const char texture_reader[] = "Microsoft.Xna.Framework.Content.Texture2DReader,";
    if (read_xnb_int(data, bytes) != 1) return false;
    unsigned name_length = read_xnb_int(data, bytes);
    if (name_length < sizeof(texture_reader) - 1 || name_length > *bytes ||
            *bytes - name_length < 4 || memcmp(*data, texture_reader, sizeof(texture_reader) - 1) != 0) {
        return false;
    }
    *data += name_length + 4;
    *bytes -= name_length + 4;
    return read_xnb_int(data, bytes) == 0 && read_xnb_int(data, bytes) == 1 && *bytes >= 20;
}

static Texture2D xnb_texture_data(const unsigned char *data, size_t bytes) {
    if (!xnb_texture_header(&data, &bytes) || read_le32(data) != 0) return (Texture2D){0};
    uint32_t width = read_le32(data + 4);
    uint32_t height = read_le32(data + 8);
    uint32_t image_bytes = read_le32(data + 16);
    if (!width || !height || width > 8192 || height > 8192 || !read_le32(data + 12) ||
            (uint64_t)width * height * 4 != image_bytes || image_bytes > bytes - 20) return (Texture2D){0};
    Image image = { .data = (void *)(data + 20), .width = (int)width, .height = (int)height,
                    .mipmaps = 1, .format = PIXELFORMAT_UNCOMPRESSED_R8G8B8A8 };
    Texture2D texture = LoadTextureFromImage(image);
    if (texture.id) SetTextureFilter(texture, TEXTURE_FILTER_POINT);
    return texture;
}

static unsigned char *read_original_xnb_bytes(const char *folder, const char *name, size_t *decoded_bytes,
        size_t prefix_limit) {
    size_t bytes = 0;
    unsigned char *file = read_asset_file(folder, name, &bytes);
    unsigned char *decoded = NULL;
    if (file && !memcmp(file, "XNBw", 4) && file[4] == 5 && read_le32(file + 6) == bytes) {
        if (!(file[5] & 0xc0)) {
            *decoded_bytes = bytes - 10;
            memmove(file, file + 10, *decoded_bytes);
            return file;
        }
        else if ((file[5] & 0xc0) == 0x80 && bytes >= 14) {
            uint32_t plain = read_le32(file + 10);
            if (plain && (plain <= asset_byte_limit || prefix_limit)) {
                size_t target = prefix_limit && prefix_limit < plain ? prefix_limit : plain;
                decoded = decode_xnb_lzx(file + 14, bytes - 14, plain, target, decoded_bytes);
            }
        }
    }
    free(file);
    return decoded;
}

static unsigned char *read_original_xnb(const char *folder, const char *name, size_t *decoded_bytes) {
    return read_original_xnb_bytes(folder, name, decoded_bytes, 0);
}

lean_obj_res terria_original_texture_size(b_lean_obj_arg asset) {
    const char *name = lean_string_cstr(asset);
    if (strncmp(name, "Images/", 7)) return lean_io_result_mk_ok(lean_box_uint64(0));
    size_t bytes = 0;
    unsigned char *payload = read_original_xnb_bytes("Images", name + 7, &bytes, 4096);
    const unsigned char *data = payload;
    uint64_t dimensions = 0;
    if (payload && xnb_texture_header(&data, &bytes)) {
        uint32_t width = read_le32(data + 4);
        uint32_t height = read_le32(data + 8);
        if (width && height && width <= 8192 && height <= 8192)
            dimensions = (uint64_t)width | ((uint64_t)height << 32);
    }
    free(payload);
    return lean_io_result_mk_ok(lean_box_uint64(dimensions));
}

static Texture2D load_original_texture(const char *name) {
    size_t bytes = 0;
    unsigned char *data = read_original_xnb("Images", name, &bytes);
    if (!data) return (Texture2D){0};
    Texture2D texture = xnb_texture_data(data, bytes);
    free(data);
    if (texture.id && (!strncmp(name, "Misc/water_", 11) || !strncmp(name, "Liquid_", 7) ||
            !strncmp(name, "LiquidSlope_", 12) || !strncmp(name, "Waterfall_", 10))) {
        SetTextureFilter(texture, TEXTURE_FILTER_BILINEAR);
        SetTextureWrap(texture, TEXTURE_WRAP_CLAMP);
    }
    return texture;
}

static Sound xnb_sound_data(const unsigned char *data, size_t bytes) {
    static const char sound_reader[] = "Microsoft.Xna.Framework.Content.SoundEffectReader";
    if (read_xnb_int(&data, &bytes) != 1) return (Sound){0};
    unsigned name_length = read_xnb_int(&data, &bytes);
    if (name_length < sizeof(sound_reader) - 1 || name_length > bytes || bytes - name_length < 4 ||
            memcmp(data, sound_reader, sizeof(sound_reader) - 1) != 0) return (Sound){0};
    data += name_length + 4;
    bytes -= name_length + 4;
    if (read_xnb_int(&data, &bytes) != 0 || read_xnb_int(&data, &bytes) != 1 || bytes < 24) return (Sound){0};
    uint32_t format_size = read_le32(data);
    if (format_size < 16 || format_size > bytes - 8) return (Sound){0};
    unsigned format = read_le16(data + 4);
    unsigned channels = read_le16(data + 6);
    unsigned rate = read_le32(data + 8);
    unsigned align = read_le16(data + 16);
    unsigned bits = read_le16(data + 18);
    uint32_t sample_bytes = read_le32(data + 4 + format_size);
    if (format != 1 || (channels != 1 && channels != 2) || bits != 16 || align != 2 * channels ||
            rate < 8000 || rate > 192000 || !sample_bytes || sample_bytes > bytes - 8 - format_size ||
            sample_bytes % align != 0) return (Sound){0};
    void *samples = malloc(sample_bytes);
    if (!samples) return (Sound){0};
    memcpy(samples, data + 8 + format_size, sample_bytes);
    Wave wave = { .frameCount = sample_bytes / align, .sampleRate = rate, .sampleSize = bits,
                  .channels = channels, .data = samples };
    Sound sound = LoadSoundFromWave(wave);
    free(samples);
    return sound;
}

static Sound load_original_sound(const char *name) {
    if (!original_content[0]) return (Sound){0};
    size_t bytes = 0;
    unsigned char *data = read_original_xnb("Sounds", name, &bytes);
    if (!data) return (Sound){0};
    Sound sound = xnb_sound_data(data, bytes);
    free(data);
    return sound;
}

static Texture2D original_texture(const char *name) {
    if (!original_content[0] || !name[0] || strchr(name, '\\') || name[0] == '/' || strstr(name, "..") ||
            strlen(name) >= asset_name_capacity) return (Texture2D){0};
    unsigned hash = 2166136261u;
    for (const unsigned char *p = (const unsigned char *)name; *p; ++p) hash = (hash ^ *p) * 16777619u;
    for (unsigned probe = 0; probe < asset_capacity; ++probe) {
        unsigned index = (hash + probe) % asset_capacity;
        if (!assets[index].name[0]) {
            strcpy(assets[index].name, name);
            assets[index].texture = load_original_texture(name);
            if (!assets[index].texture.id) {
                TraceLog(LOG_WARNING, "Could not load the Terraria texture %s.xnb", name);
            }
            return assets[index].texture;
        }
        if (!strcmp(assets[index].name, name)) return assets[index].texture;
    }
    return (Texture2D){0};
}

static void open_originals(void) {
    const char *override = getenv("LEAN4TERRIA_ORIGINAL_CONTENT");
    const char *home = getenv("HOME");
    if (override && !*override) return;
    if (!override && !home) return;
    int size = override ? snprintf(original_content, sizeof(original_content), "%s", override)
        : snprintf(original_content, sizeof(original_content),
            "%s/Library/Application Support/Steam/steamapps/common/Terraria/Terraria.app/Contents/Resources/Content",
            home);
    if (size < 0 || (size_t)size >= sizeof(original_content) ||
            access(original_content, R_OK) != 0 || !open_cabinet_library()) {
        original_content[0] = 0;
        return;
    }
    static const char *required[] = {
        "Logo", "Inventory_Back", "Player_0_0", "Tiles_0", "Tiles_1", "Heart"
    };
    for (size_t i = 0; i < sizeof(required) / sizeof(required[0]); ++i) {
        if (!original_texture(required[i]).id) {
            original_content[0] = 0;
            return;
        }
    }
    Texture2D pixel = original_texture("UI/OnePixel");
    original_cursor = original_texture("UI/Cursor_0");
    if (!pixel.id || !original_cursor.id) {
        original_content[0] = 0;
        return;
    }
    SetShapesTexture(pixel, (Rectangle){0, 0, pixel.width, pixel.height});

    size_t bytes = 0;
    unsigned char *font = read_original_xnb("Fonts", "Mouse_Text", &bytes);
    if (!font) return;
    load_terraria_font(&terraria_font, font, bytes, false);
    // Load the first page for ordinary HUD text. The rename keyboard can load more glyphs on demand.
    font = read_original_xnb_bytes("Fonts", "Death_Text", &bytes, 1024 * 1024);
    load_terraria_font(&death_font, font, bytes, true);
}

static bool death_font_upgrade_tried;

static TerrariaFont *rename_font_for(const char *text) {
    if (death_font.glyph && death_font.page_count == 1 && !death_font_upgrade_tried) {
        unsigned second = 0;
        while (*text || second) {
            unsigned letter = next_font_unit(&text, &second);
            if (death_font.glyph[letter].present || letter < 0x80 || (letter >= 0xd800 && letter <= 0xdfff))
                continue;
            death_font_upgrade_tried = true;
            size_t bytes = 0;
            unsigned char *payload = read_original_xnb_bytes("Fonts", "Death_Text", &bytes, 128 * 1024 * 1024);
            load_terraria_font(&death_font, payload, bytes, false);
            break;
        }
    }
    return death_font.glyph ? &death_font : &terraria_font;
}

static void close_originals(void) {
    if (magic_missile_shader.id) UnloadShader(magic_missile_shader);
    magic_missile_shader = (Shader){0};
    magic_missile_shader_tried = false;
    if (queen_slime_shader.id) UnloadShader(queen_slime_shader);
    queen_slime_shader = (Shader){0};
    queen_slime_shader_tried = false;
    unload_terraria_font(&terraria_font);
    unload_terraria_font(&death_font);
    unload_terraria_font(&item_stack_font);
    unload_terraria_font(&combat_text_font);
    unload_terraria_font(&combat_crit_font);
    death_font_upgrade_tried = false;
    for (unsigned index = 0; index < asset_capacity; ++index) {
        if (assets[index].texture.id) UnloadTexture(assets[index].texture);
    }
    memset(assets, 0, sizeof(assets));
    original_cursor = (Texture2D){0};
    original_content[0] = 0;
    if (archive_library) dlclose(archive_library);
    archive_library = NULL;
}

// Cocoa requires the process main thread. Lean normally starts main on a worker.
__attribute__((constructor)) static void use_process_main_thread(void) {
    setenv("LEAN_MAIN_USE_THREAD", "0", 1);
}

enum { break_sound, place_sound, craft_sound, swing_sound, hurt_sound,
       warden_windup_sound, warden_defeat_sound, reward_sound, bow_sound, barrage_sound,
       beacon_bind_sound, recall_sound, catch_sound, drink_sound,
       grapple_cast_sound, grapple_latch_sound, crossbow_sound, eye_summon_sound,
       eye_transform_sound, eye_defeat_sound, eye_charge_sound,
       magic_missile_cast_sound, magic_missile_impact_sound,
       valor_cast_sound, handgun_fire_sound, underworld_magic_cast_sound, hellwing_fire_sound,
       underworld_npc_sound, wall_roar_sound, wall_hit_sound, wall_death_sound,
       wraith_hit_sound, wraith_death_sound, twins_laser_sound, twins_flame_sound,
       twins_hit_sound, twins_death_sound, bobber_splash_sound, fishing_line_snap_sound,
       fishing_pickup_sound, winged_slime_mount_sound, blade_staff_sound,
       blade_recoil_0, blade_recoil_1, blade_recoil_2, destroyer_burrow_sound,
       destroyer_hit_sound, destroyer_death_sound, probe_laser_sound,
       prime_projectile_sound, prime_blast_sound, plantera_death_sound,
       temple_unlock_sound, temple_door_open_sound, temple_door_close_sound,
       lihzahrd_hit_sound, lihzahrd_death_sound, flying_snake_hit_sound, flying_snake_death_sound,
       chest_open_sound, chest_close_sound, menu_tick_sound,
       bucket_splash_sound, player_hit_sound, player_death_sound, chat_open_sound,
       angry_bones_hit_sound, angry_bones_killed_sound,
       water_sphere_hit_sound, water_sphere_killed_sound,
       sound_count };

static Sound sounds[sound_count];
static Sound original_mana_refill;
static Sound original_tile_dig[3];
static Sound original_tink[3];
static Sound original_item_swing[3];
static Sound original_snow_ice[3];
static Sound original_snow_ice_instances[3];
static Sound original_pickup_grab;
static Sound original_grab_instance;
/* Player.PlayHurtSound: male (type 1), female (type 20), default voice (type 42 style 317). */
static Sound original_player_hit[3];
static Sound original_player_hit_instances[3];
static Sound original_female_hit[3];
static Sound original_female_hit_instances[3];
static Sound original_player_hit_default;
/* NPCHit1 (type 3 style 1) stops its previous instance; NPCDeath1 (type 4 style 1) overlaps. */
enum { npc_sound_styles = 66 };
static Sound original_npc_hit[npc_sound_styles];            /* NPC_Hit_N, loaded on first use */
static Sound original_npc_hit_instance[npc_sound_styles];
static Sound original_npc_killed[npc_sound_styles];         /* NPC_Killed_N, loaded on first use */
static bool original_npc_hit_tried[npc_sound_styles];
static bool original_npc_killed_tried[npc_sound_styles];
/* Player_Killed (type 5): one slot, the previous instance is replaced. */
static Sound original_player_killed_instance;
/* Zombie_0..2 (type 14 style 1): the ambient groan of fighter NPCs; instances overlap. */
static Sound original_zombie[3];
static Sound original_coin[5];
static Sound *original_sound_instances;
static size_t original_sound_instance_count;

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

static Sound original_sound_or_tone(const char *name, double frequency, double seconds, double glide) {
    Sound sound = load_original_sound(name);
    return IsSoundValid(sound) ? sound : tone(frequency, seconds, glide);
}

lean_obj_res terria_open(uint32_t width, uint32_t height, b_lean_obj_arg title) {
    SetTraceLogLevel(LOG_WARNING);
    InitWindow((int)width, (int)height, lean_string_cstr(title));
    if (!IsWindowReady()) {
        return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string("Could not open the raylib window.")));
    }
    SetExitKey(KEY_NULL);
    SetTargetFPS(60);
    open_originals();
    if (!original_content[0] || !terraria_font.glyph) {
        close_originals();
        CloseWindow();
        return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(
            "Terraria textures or Mouse_Text could not be loaded. Install Terraria in Steam or set "
            "LEAN4TERRIA_ORIGINAL_CONTENT to Terraria.app/Contents/Resources/Content.")));
    }
    HideCursor();
    InitAudioDevice();
    if (IsAudioDeviceReady()) {
        sounds[break_sound] = tone(154.0, 0.09, -620.0);
        sounds[place_sound] = tone(220.0, 0.08, -920.0);
        sounds[craft_sound] = tone(410.0, 0.17, 1800.0);
        sounds[swing_sound] = tone(272.0, 0.10, -1600.0);
        sounds[hurt_sound] = tone(164.0, 0.15, -700.0);
        sounds[warden_windup_sound] = tone(196.0, 0.17, 1500.0);
        sounds[warden_defeat_sound] = tone(420.0, 0.18, -1400.0);
        sounds[reward_sound] = tone(620.0, 0.16, 2000.0);
        sounds[bow_sound] = tone(338.0, 0.07, -3450.0);
        sounds[barrage_sound] = tone(191.0, 0.13, -1040.0);
        sounds[beacon_bind_sound] = tone(540.0, 0.14, 1600.0);
        sounds[recall_sound] = tone(820.0, 0.18, -2600.0);
        sounds[catch_sound] = tone(720.0, 0.10, 2300.0);
        sounds[drink_sound] = tone(415.0, 0.14, 1800.0);
        sounds[grapple_cast_sound] = tone(293.0, 0.10, 2400.0);
        sounds[grapple_latch_sound] = tone(680.0, 0.08, -4800.0);
        sounds[crossbow_sound] = tone(222.0, 0.11, -1250.0);
        sounds[eye_summon_sound] = tone(108.0, 0.18, 325.0);
        sounds[eye_transform_sound] = tone(185.0, 0.18, -660.0);
        sounds[eye_defeat_sound] = tone(258.0, 0.18, 1120.0);
        sounds[eye_charge_sound] = tone(231.0, 0.09, -1760.0);
        sounds[magic_missile_cast_sound] = original_sound_or_tone("Item_9", 605.0, 0.11, 1500.0);
        sounds[magic_missile_impact_sound] = original_sound_or_tone("Item_10", 310.0, 0.17, -1550.0);
        sounds[valor_cast_sound] = original_sound_or_tone("Item_1", 272.0, 0.10, -1600.0);
        sounds[handgun_fire_sound] = original_sound_or_tone("Item_41", 147.0, 0.065, -1800.0);
        sounds[underworld_magic_cast_sound] = original_sound_or_tone("Item_20", 480.0, 0.13, -2100.0);
        sounds[hellwing_fire_sound] = original_sound_or_tone("Item_5", 338.0, 0.07, -3450.0);
        sounds[underworld_npc_sound] = original_sound_or_tone("Item_8", 451.0, 0.13, -1900.0);
        sounds[wall_roar_sound] = original_sound_or_tone("Roar_0", 108.0, 0.18, 325.0);
        sounds[wall_hit_sound] = original_sound_or_tone("NPC_Hit_8", 186.0, 0.12, -710.0);
        sounds[wall_death_sound] = original_sound_or_tone("NPC_Killed_10", 110.0, 0.18, -460.0);
        sounds[wraith_hit_sound] = original_sound_or_tone("NPC_Hit_54", 190.0, 0.11, -580.0);
        sounds[wraith_death_sound] = original_sound_or_tone("NPC_Killed_52", 150.0, 0.17, -420.0);
        sounds[twins_laser_sound] = original_sound_or_tone("Item_33", 450.0, 0.10, 1380.0);
        sounds[twins_flame_sound] = original_sound_or_tone("Item_34", 330.0, 0.12, -1650.0);
        sounds[twins_hit_sound] = original_sound_or_tone("NPC_Hit_1", 185.0, 0.11, -710.0);
        sounds[twins_death_sound] = original_sound_or_tone("NPC_Killed_14", 140.0, 0.21, -390.0);
        sounds[bobber_splash_sound] = original_sound_or_tone("Item_19", 260.0, 0.13, -2300.0);
        sounds[fishing_line_snap_sound] = original_sound_or_tone("Item_17", 480.0, 0.10, -3200.0);
        sounds[fishing_pickup_sound] = original_sound_or_tone("Grab", 720.0, 0.10, 2300.0);
        sounds[winged_slime_mount_sound] = load_original_sound("Item_25");
        sounds[blade_staff_sound] = load_original_sound("Item_44");
        sounds[blade_recoil_0] = load_original_sound("Dig_0");
        sounds[blade_recoil_1] = load_original_sound("Dig_1");
        sounds[blade_recoil_2] = load_original_sound("Dig_2");
        sounds[destroyer_burrow_sound] = load_original_sound("Roar_1");
        sounds[destroyer_hit_sound] = load_original_sound("NPC_Hit_4");
        sounds[destroyer_death_sound] = load_original_sound("NPC_Killed_14");
        sounds[probe_laser_sound] = load_original_sound("Item_12");
        sounds[prime_projectile_sound] = load_original_sound("Item_10");
        sounds[prime_blast_sound] = load_original_sound("Item_14");
        sounds[plantera_death_sound] = load_original_sound("NPC_Killed_1");
        sounds[temple_unlock_sound] = load_original_sound("Unlock");
        sounds[temple_door_open_sound] = load_original_sound("Door_Opened");
        sounds[temple_door_close_sound] = load_original_sound("Door_Closed");
        sounds[lihzahrd_hit_sound] = load_original_sound("NPC_Hit_26");
        sounds[lihzahrd_death_sound] = load_original_sound("NPC_Killed_29");
        sounds[flying_snake_hit_sound] = load_original_sound("NPC_Hit_23");
        sounds[flying_snake_death_sound] = load_original_sound("NPC_Killed_26");
        sounds[angry_bones_hit_sound] = load_original_sound("NPC_Hit_2");
        sounds[angry_bones_killed_sound] = load_original_sound("NPC_Killed_2");
        sounds[water_sphere_hit_sound] = load_original_sound("NPC_Hit_3");
        sounds[water_sphere_killed_sound] = load_original_sound("NPC_Killed_3");
        sounds[chest_open_sound] = load_original_sound("Menu_Open");
        sounds[chest_close_sound] = load_original_sound("Menu_Close");
        sounds[menu_tick_sound] = load_original_sound("Menu_Tick");
        sounds[bucket_splash_sound] = load_original_sound("Splash_1");
        sounds[player_hit_sound] = load_original_sound("Player_Hit_0");
        sounds[player_death_sound] = load_original_sound("Player_Killed");
        sounds[chat_open_sound] = load_original_sound("Chat");
        original_mana_refill = load_original_sound("MaxMana");
        original_pickup_grab = load_original_sound("Grab");
        for (int i = 0; i < 5; ++i) {
            char name[16];
            snprintf(name, sizeof name, "Coin_%d", i);
            original_coin[i] = load_original_sound(name);
        }
        for (int i = 0; i < 3; ++i) {
            char name[16];
            snprintf(name, sizeof name, "Dig_%d", i);
            original_tile_dig[i] = load_original_sound(name);
            snprintf(name, sizeof name, "Tink_%d", i);
            original_tink[i] = load_original_sound(name);
            static const int swing_styles[3] = {1, 18, 19};
            snprintf(name, sizeof name, "Item_%d", swing_styles[i]);
            original_item_swing[i] = load_original_sound(name);
            snprintf(name, sizeof name, "Item_%d", 48 + i);
            original_snow_ice[i] = load_original_sound(name);
            snprintf(name, sizeof name, "Player_Hit_%d", i);
            original_player_hit[i] = load_original_sound(name);
            snprintf(name, sizeof name, "Female_Hit_%d", i);
            original_female_hit[i] = load_original_sound(name);
        }
        original_player_hit_default = load_original_sound("Custom/player_hit_default");
        for (int i = 0; i < 3; ++i) {
            char name[16];
            snprintf(name, sizeof name, "Zombie_%d", i);
            original_zombie[i] = load_original_sound(name);
        }
    }
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_close(void) {
    for (size_t i = 0; i < 2; ++i) {
        if (original_liquid_layers[i].id) UnloadRenderTexture(original_liquid_layers[i]);
        original_liquid_layers[i] = (RenderTexture2D){0};
    }
    ShowCursor();
    unload_boss_music(&wall_music);
    unload_boss_music(&twins_music);
    unload_boss_music(&destroyer_music);
    unload_boss_music(&prime_music);
    unload_boss_music(&plantera_music);
    unload_boss_music(&golem_music);
    unload_boss_music(&temple_music);
    unload_boss_music(&dungeon_music);
    unload_boss_music(&skeletron_music);
    close_originals();
    if (IsAudioDeviceReady()) {
        for (size_t i = 0; i < original_sound_instance_count; ++i)
            UnloadSoundAlias(original_sound_instances[i]);
        free(original_sound_instances);
        original_sound_instances = NULL;
        original_sound_instance_count = 0;
        if (IsSoundValid(original_grab_instance)) UnloadSoundAlias(original_grab_instance);
        original_grab_instance = (Sound){0};
        if (IsSoundValid(original_pickup_grab)) UnloadSound(original_pickup_grab);
        original_pickup_grab = (Sound){0};
        for (int i = 0; i < 5; ++i) {
            if (IsSoundValid(original_coin[i])) UnloadSound(original_coin[i]);
            original_coin[i] = (Sound){0};
        }
        if (IsSoundValid(original_mana_refill)) UnloadSound(original_mana_refill);
        original_mana_refill = (Sound){0};
        if (IsSoundValid(original_player_hit_default)) UnloadSound(original_player_hit_default);
        original_player_hit_default = (Sound){0};
        for (int i = 0; i < 3; ++i) {
            if (IsSoundValid(original_zombie[i])) UnloadSound(original_zombie[i]);
            original_zombie[i] = (Sound){0};
        }
        if (IsSoundValid(original_player_killed_instance)) UnloadSoundAlias(original_player_killed_instance);
        original_player_killed_instance = (Sound){0};
        for (int i = 0; i < npc_sound_styles; ++i) {
            if (IsSoundValid(original_npc_hit_instance[i])) UnloadSoundAlias(original_npc_hit_instance[i]);
            original_npc_hit_instance[i] = (Sound){0};
            if (IsSoundValid(original_npc_hit[i])) UnloadSound(original_npc_hit[i]);
            original_npc_hit[i] = (Sound){0};
            if (IsSoundValid(original_npc_killed[i])) UnloadSound(original_npc_killed[i]);
            original_npc_killed[i] = (Sound){0};
            original_npc_hit_tried[i] = original_npc_killed_tried[i] = false;
        }
        for (int i = 0; i < 3; ++i) {
            if (IsSoundValid(original_player_hit_instances[i])) UnloadSoundAlias(original_player_hit_instances[i]);
            original_player_hit_instances[i] = (Sound){0};
            if (IsSoundValid(original_player_hit[i])) UnloadSound(original_player_hit[i]);
            original_player_hit[i] = (Sound){0};
            if (IsSoundValid(original_female_hit_instances[i])) UnloadSoundAlias(original_female_hit_instances[i]);
            original_female_hit_instances[i] = (Sound){0};
            if (IsSoundValid(original_female_hit[i])) UnloadSound(original_female_hit[i]);
            original_female_hit[i] = (Sound){0};
            if (IsSoundValid(original_snow_ice_instances[i])) UnloadSoundAlias(original_snow_ice_instances[i]);
            original_snow_ice_instances[i] = (Sound){0};
            if (IsSoundValid(original_snow_ice[i])) UnloadSound(original_snow_ice[i]);
            original_snow_ice[i] = (Sound){0};
            if (IsSoundValid(original_tile_dig[i])) UnloadSound(original_tile_dig[i]);
            original_tile_dig[i] = (Sound){0};
            if (IsSoundValid(original_tink[i])) UnloadSound(original_tink[i]);
            original_tink[i] = (Sound){0};
            if (IsSoundValid(original_item_swing[i])) UnloadSound(original_item_swing[i]);
            original_item_swing[i] = (Sound){0};
        }
        for (int i = 0; i < sound_count; ++i) {
            if (IsSoundValid(sounds[i])) UnloadSound(sounds[i]);
        }
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

lean_obj_res terria_window_focused(void) {
    return lean_io_result_mk_ok(lean_box(IsWindowFocused()));
}

static uint64_t terria_input_bits(bool original_world) {
    uint64_t bits = 0;
    if (IsKeyDown(KEY_A) || IsKeyDown(KEY_LEFT)) bits |= UINT64_C(1) << 0;
    if (IsKeyDown(KEY_D) || IsKeyDown(KEY_RIGHT)) bits |= UINT64_C(1) << 1;
    if (IsKeyPressed(KEY_SPACE)) bits |= UINT64_C(1) << 2;
    if (IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) bits |= UINT64_C(1) << 3;
    if (IsMouseButtonPressed(MOUSE_BUTTON_RIGHT)) bits |= UINT64_C(1) << 4;
    if (IsKeyPressed(KEY_E)) bits |= UINT64_C(1) << 5;
    if ((!original_world && IsKeyPressed(KEY_S)) || IsKeyPressed(KEY_F5)) bits |= UINT64_C(1) << 6;
    if (IsKeyPressed(KEY_L) || IsKeyPressed(KEY_F9)) bits |= UINT64_C(1) << 7;
    for (int index = 0; index < 8; ++index) {
        if (IsKeyPressed(KEY_ONE + index)) bits |= UINT64_C(1) << (8 + index);
    }
    if (IsKeyPressed(KEY_F)) bits |= UINT64_C(1) << 16;
    if (IsKeyPressed(KEY_R)) bits |= UINT64_C(1) << 17;
    if (IsKeyPressed(KEY_TAB)) bits |= UINT64_C(1) << 18;
    if (IsKeyPressed(KEY_T)) bits |= UINT64_C(1) << 19;
    if (IsKeyPressed(KEY_G)) bits |= UINT64_C(1) << 20;
    if (original_world ? IsKeyDown(KEY_Q) : IsKeyPressed(KEY_Q)) bits |= UINT64_C(1) << 21;
    if (IsKeyPressed(KEY_H)) bits |= UINT64_C(1) << 22;
    if (IsKeyPressed(KEY_B)) bits |= UINT64_C(1) << 23;
    if (IsKeyPressed(KEY_C)) bits |= UINT64_C(1) << 24;
    if (IsKeyPressed(KEY_V)) bits |= UINT64_C(1) << 25;
    if (IsKeyDown(KEY_SPACE)) bits |= UINT64_C(1) << 26;
    if (IsKeyPressed(KEY_Y)) bits |= UINT64_C(1) << 27;
    if (IsKeyPressed(KEY_U)) bits |= UINT64_C(1) << 28;
    if (IsKeyPressed(KEY_I)) bits |= UINT64_C(1) << 29;
    if (IsKeyPressed(KEY_M)) bits |= UINT64_C(1) << 30;
    if (IsKeyPressed(KEY_J)) bits |= UINT64_C(1) << 31;
    if (IsKeyPressed(KEY_N)) bits |= UINT64_C(1) << 32;
    if (IsKeyPressed(KEY_ESCAPE)) bits |= UINT64_C(1) << 33;
    if (IsKeyPressed(KEY_ENTER) || IsKeyPressed(KEY_KP_ENTER)) bits |= UINT64_C(1) << 34;
    if (IsKeyPressed(KEY_UP)) bits |= UINT64_C(1) << 35;
    if (IsKeyPressed(KEY_DOWN)) bits |= UINT64_C(1) << 36;
    if (IsKeyPressed(KEY_BACKSPACE)) bits |= UINT64_C(1) << 37;
    if (IsKeyPressed(KEY_F1)) bits |= UINT64_C(1) << 38;
    if (IsKeyPressed(KEY_LEFT)) bits |= UINT64_C(1) << 39;
    if (IsKeyPressed(KEY_RIGHT)) bits |= UINT64_C(1) << 40;
    if (IsKeyPressed(KEY_P)) bits |= UINT64_C(1) << 41;
    if (IsKeyPressed(KEY_K)) bits |= UINT64_C(1) << 42;
    if (IsKeyPressed(KEY_Z)) bits |= UINT64_C(1) << 43;
    if (IsKeyDown(KEY_DOWN) || (original_world && IsKeyDown(KEY_S))) bits |= UINT64_C(1) << 44;
    if (IsKeyPressed(KEY_X)) bits |= UINT64_C(1) << 45;
    if (IsKeyPressed(KEY_O)) bits |= UINT64_C(1) << 46;
    if (IsKeyPressed(KEY_W)) bits |= UINT64_C(1) << 47;
    if (IsKeyPressed(KEY_F2)) bits |= UINT64_C(1) << 48;
    if (IsKeyPressed(KEY_F3)) bits |= UINT64_C(1) << 49;
    if (IsKeyPressed(KEY_F4)) bits |= UINT64_C(1) << 50;
    if (IsKeyPressed(KEY_F8)) bits |= UINT64_C(1) << 51;
    if (IsKeyPressed(KEY_F6)) bits |= UINT64_C(1) << 52;
    if (IsKeyPressed(KEY_F7)) bits |= UINT64_C(1) << 53;
    if (IsKeyPressed(KEY_F10)) bits |= UINT64_C(1) << 54;
    if (IsKeyDown(KEY_UP)) bits |= UINT64_C(1) << 55;
    if (original_world ? IsMouseButtonDown(MOUSE_BUTTON_RIGHT) : IsKeyPressed(KEY_F11)) {
        bits |= UINT64_C(1) << 56;
    }
    if (IsMouseButtonDown(MOUSE_BUTTON_LEFT)) bits |= UINT64_C(1) << 57;
    if (IsKeyPressed(KEY_NINE)) bits |= UINT64_C(1) << 58;
    if (IsKeyPressed(KEY_ZERO)) bits |= UINT64_C(1) << 59;
    float wheel = GetMouseWheelMove();
    if (wheel > 0) bits |= UINT64_C(1) << 60;
    if (wheel < 0) bits |= UINT64_C(1) << 61;
    if (IsKeyDown(KEY_LEFT_CONTROL) || IsKeyDown(KEY_RIGHT_CONTROL) ||
            IsKeyDown(KEY_LEFT_SUPER) || IsKeyDown(KEY_RIGHT_SUPER)) bits |= UINT64_C(1) << 62;
    if (IsKeyDown(KEY_LEFT_SHIFT) || IsKeyDown(KEY_RIGHT_SHIFT)) bits |= UINT64_C(1) << 63;
    return bits;
}

lean_obj_res terria_input(void) {
    return lean_io_result_mk_ok(lean_box_uint64(terria_input_bits(false)));
}

lean_obj_res terria_original_input(void) {
    return lean_io_result_mk_ok(lean_box_uint64(terria_input_bits(true)));
}

/* Bit i: the number key of hotbar slot i is down (1..9 then 0), as Triggers.Current.HotbarN. */
lean_obj_res terria_hotbar_held(void) {
    uint32_t bits = 0;
    for (int index = 0; index < 9; ++index) {
        if (IsKeyDown(KEY_ONE + index)) bits |= 1u << index;
    }
    if (IsKeyDown(KEY_ZERO)) bits |= 1u << 9;
    return lean_io_result_mk_ok(lean_box_uint32(bits));
}

lean_obj_res terria_read_character(void) {
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)GetCharPressed()));
}

lean_obj_res terria_rename_keys(void) {
    uint32_t keys = 0;
    if (IsKeyDown(KEY_LEFT_CONTROL) || IsKeyDown(KEY_RIGHT_CONTROL)) keys |= 1u << 0;
    if (IsKeyDown(KEY_LEFT_ALT) || IsKeyDown(KEY_RIGHT_ALT)) keys |= 1u << 1;
    if (IsKeyDown(KEY_LEFT_SHIFT) || IsKeyDown(KEY_RIGHT_SHIFT)) keys |= 1u << 2;
    if (IsKeyDown(KEY_Z)) keys |= 1u << 3;
    if (IsKeyDown(KEY_X)) keys |= 1u << 4;
    if (IsKeyDown(KEY_C)) keys |= 1u << 5;
    if (IsKeyDown(KEY_V)) keys |= 1u << 6;
    if (IsKeyDown(KEY_INSERT)) keys |= 1u << 7;
    if (IsKeyDown(KEY_DELETE)) keys |= 1u << 8;
    if (IsKeyDown(KEY_ESCAPE)) keys |= 1u << 9;
    if (IsKeyDown(KEY_BACKSPACE)) keys |= 1u << 10;
    return lean_io_result_mk_ok(lean_box_uint32(keys));
}

lean_obj_res terria_guide_grid_wheel(void) {
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)(int32_t)(GetMouseWheelMove() * 120.0f)));
}

lean_obj_res terria_dash_tap(void) {
    bool left = IsKeyPressed(KEY_A) || IsKeyPressed(KEY_LEFT);
    bool right = IsKeyPressed(KEY_D) || IsKeyPressed(KEY_RIGHT);
    return lean_io_result_mk_ok(lean_box_uint32(left == right ? 0 : right ? 1 : 2));
}

lean_obj_res terria_mouse_x(void) {
    int x = GetMouseX();
    return lean_io_result_mk_ok(lean_box_uint32(x < 0 ? 0 : (uint32_t)x));
}

lean_obj_res terria_mouse_y(void) {
    int y = GetMouseY();
    return lean_io_result_mk_ok(lean_box_uint32(y < 0 ? 0 : (uint32_t)y));
}

lean_obj_res terria_begin(uint32_t color) {
    BeginDrawing();
    original_cursor_layer_drawn = false;
    ClearBackground(GetColor(color));
    return lean_io_result_mk_ok(lean_box(0));
}

/* Acceptance aid, off unless LEAN4TERRIA_CAPTURE_DIR names a directory: when a file called
   "request" appears there, the finished frame is written beside it as capture-NNN.png. */
static void terria_capture_frame(void) {
    static const char *directory = NULL;
    static bool checked = false;
    static int index = 0;
    if (!checked) {
        directory = getenv("LEAN4TERRIA_CAPTURE_DIR");
        checked = true;
    }
    if (!directory || !directory[0]) return;
    char request[PATH_MAX];
    snprintf(request, sizeof request, "%s/request", directory);
    if (access(request, F_OK) != 0) return;
    rlDrawRenderBatchActive();
    Image image = LoadImageFromScreen();
    char path[PATH_MAX];
    snprintf(path, sizeof path, "%s/capture-%03d.png", directory, index++);
    ExportImage(image, path);
    UnloadImage(image);
    unlink(request);
}

/* Explicit UI layer for the original-world client. Legacy callers retain their
   EndDrawing fallback. The pointer palette/border still uses the existing backend. */
lean_obj_res terria_original_cursor_layer(float scale) {
    original_cursor_layer_drawn = true;
    if (IsCursorOnScreen()) {
        Vector2 mouse = GetMousePosition();
        Vector2 shadow = {mouse.x + 1, mouse.y + 1};
        DrawTextureEx(original_cursor, shadow, 0, scale * 1.1f, (Color){50, 50, 50, 128});
        DrawTextureEx(original_cursor, mouse, 0, scale, WHITE);
    }
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_original_cursor_layer_handled(void) {
    original_cursor_layer_drawn = true;
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_original_pointer_position(void) {
    uint64_t x = (uint32_t)GetMouseX();
    uint64_t y = (uint32_t)GetMouseY();
    return lean_io_result_mk_ok(lean_box_uint64((x << 32) | y));
}

lean_obj_res terria_end(void) {
    if (!original_cursor_layer_drawn) {
        lean_dec(terria_original_cursor_layer(1.0f));
    }
    terria_capture_frame();
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

lean_obj_res terria_clip_begin(uint32_t x, uint32_t y, uint32_t width, uint32_t height) {
    BeginScissorMode((int)x, (int)y, (int)width, (int)height);
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_clip_end(void) {
    EndScissorMode();
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_clipboard(b_lean_obj_arg value) {
    SetClipboardText(lean_string_cstr(value));
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_read_clipboard(void) {
    const char *text = GetClipboardText();
    return lean_io_result_mk_ok(lean_mk_string(text ? text : ""));
}

lean_obj_res terria_map_input(void) {
    uint64_t bits = 0;
    if (IsKeyDown(KEY_LEFT)) bits |= 1u << 0;
    if (IsKeyDown(KEY_RIGHT)) bits |= 1u << 1;
    if (IsKeyDown(KEY_UP)) bits |= 1u << 2;
    if (IsKeyDown(KEY_DOWN)) bits |= 1u << 3;
    if (IsKeyDown(KEY_EQUAL) || IsKeyDown(KEY_KP_ADD)) bits |= 1u << 4;
    if (IsKeyDown(KEY_MINUS) || IsKeyDown(KEY_KP_SUBTRACT)) bits |= 1u << 5;
    if (IsKeyDown(KEY_PAGE_UP)) bits |= 1u << 6;
    if (IsKeyDown(KEY_PAGE_DOWN)) bits |= 1u << 7;
    return lean_io_result_mk_ok(lean_box_uint64(bits));
}

lean_obj_res terria_text_width(b_lean_obj_arg text, uint32_t size) {
    float width = terraria_font_width(&terraria_font, lean_string_cstr(text));
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)ceilf(width * size / 16.0f)));
}

lean_obj_res terria_text(b_lean_obj_arg text, uint32_t x, uint32_t y, uint32_t size, uint32_t color) {
    const char *label = lean_string_cstr(text);
    Color tint = GetColor(color);
    draw_terraria_font(&terraria_font, label, (int32_t)x, (int32_t)y, size / 16.0f, tint);
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_original_sign_text(b_lean_obj_arg text, double x, double y, double scale,
        uint32_t color, uint32_t border) {
    const char *label = lean_string_cstr(text);
    Color shadow = GetColor(border);
    BeginBlendMode(BLEND_ALPHA_PREMULTIPLY);
    draw_terraria_font(&terraria_font, label, (float)x - 2, (float)y, (float)scale, shadow);
    draw_terraria_font(&terraria_font, label, (float)x + 2, (float)y, (float)scale, shadow);
    draw_terraria_font(&terraria_font, label, (float)x, (float)y - 2, (float)scale, shadow);
    draw_terraria_font(&terraria_font, label, (float)x, (float)y + 2, (float)scale, shadow);
    draw_terraria_font(&terraria_font, label, (float)x, (float)y, (float)scale, GetColor(color));
    EndBlendMode();
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_death_text(b_lean_obj_arg text, uint32_t center, uint32_t y, double scale, uint32_t color) {
    const char *label = lean_string_cstr(text);
    TerrariaFont *font = &death_font;
    if (!font->glyph) {
        font = &terraria_font;
        scale *= 2.5;
    }
    float x = center - terraria_font_width(font, label) * (float)scale * 0.5f;
    BeginBlendMode(BLEND_ALPHA_PREMULTIPLY);
    draw_terraria_font(font, label, x, (int32_t)y, (float)scale, GetColor(color));
    EndBlendMode();
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_rename_text_width(b_lean_obj_arg text) {
    const char *label = lean_string_cstr(text);
    TerrariaFont *font = rename_font_for(label);
    float scale = font == &death_font ? 1 : 2.5f;
    return lean_io_result_mk_ok(lean_box_float(terraria_font_width(font, label) * scale));
}

lean_obj_res terria_rename_text(b_lean_obj_arg text, double x, double y, double scale, uint32_t color) {
    const char *label = lean_string_cstr(text);
    TerrariaFont *font = rename_font_for(label);
    if (font != &death_font) scale *= 2.5;
    BeginBlendMode(BLEND_ALPHA_PREMULTIPLY);
    const float shadow = 1.5f;
    draw_terraria_font(font, label, (float)x - shadow, (float)y, (float)scale, BLACK);
    draw_terraria_font(font, label, (float)x + shadow, (float)y, (float)scale, BLACK);
    draw_terraria_font(font, label, (float)x, (float)y - shadow, (float)scale, BLACK);
    draw_terraria_font(font, label, (float)x, (float)y + shadow, (float)scale, BLACK);
    draw_terraria_font(font, label, (float)x, (float)y, (float)scale, GetColor(color));
    EndBlendMode();
    return lean_io_result_mk_ok(lean_box(0));
}

/* Queen Slime's NPC atlas contains a mask. The original effect uses the
   color gradient in Extra_180 and the noise in Extra_179. */
static bool begin_queen_slime_shader(bool mount) {
    if (!queen_slime_shader_tried) {
        static const char fragment[] =
            "#version 330\n"
            "in vec2 fragTexCoord; in vec4 fragColor; out vec4 finalColor;\n"
            "uniform sampler2D texture0, palette, noiseMap; uniform float clockPhase; uniform vec2 frames;\n"
            "void main() {\n"
            "  vec4 mask = texture(texture0, fragTexCoord);\n"
            "  if (mask.a == 0.0) discard;\n"
            "  vec2 uv = fract(fragTexCoord * frames);\n"
            "  float noise = texture(noiseMap, fract(uv * 0.6 + vec2(clockPhase * 0.35, clockPhase))).r;\n"
            "  float hue = fract(0.72 * uv.y + 0.36 * uv.x + 0.32 * noise - clockPhase);\n"
            "  vec3 gel = texture(palette, vec2(hue, 0.5)).rgb;\n"
            "  float gleam = smoothstep(0.20, 0.60, mask.r / mask.a);\n"
            "  vec3 color = mix(gel * (0.58 + 0.42 * noise), vec3(1.0, 0.91, 0.99), gleam * 0.75);\n"
            "  finalColor = vec4(color * fragColor.rgb, mask.a * fragColor.a);\n"
            "}\n";
        queen_slime_shader_tried = true;
        queen_slime_shader = LoadShaderFromMemory(NULL, fragment);
    }
    if (!queen_slime_shader.id) return false;
    int palette_loc = GetShaderLocation(queen_slime_shader, "palette");
    int noise_loc = GetShaderLocation(queen_slime_shader, "noiseMap");
    int clock_loc = GetShaderLocation(queen_slime_shader, "clockPhase");
    int frames_loc = GetShaderLocation(queen_slime_shader, "frames");
    if (palette_loc < 0 || noise_loc < 0 || clock_loc < 0 || frames_loc < 0) return false;
    Texture2D palette = original_texture("Extra_180");
    Texture2D noise = original_texture("Extra_179");
    if (!palette.id || !noise.id) return false;
    float phase = (float)fmod(GetTime() / 6.0, 1.0);
    float frames[] = {mount ? 1.0f : 2.0f, mount ? 8.0f : 16.0f};
    BeginShaderMode(queen_slime_shader);
    SetShaderValueTexture(queen_slime_shader, palette_loc, palette);
    SetShaderValueTexture(queen_slime_shader, noise_loc, noise);
    SetShaderValue(queen_slime_shader, clock_loc, &phase, SHADER_UNIFORM_FLOAT);
    SetShaderValue(queen_slime_shader, frames_loc, frames, SHADER_UNIFORM_VEC2);
    return true;
}

lean_obj_res terria_original_sprite(b_lean_obj_arg name, uint32_t sx, uint32_t sy, uint32_t sw, uint32_t sh,
        uint32_t x, uint32_t y, uint32_t width, uint32_t height, uint32_t color, uint32_t flags, double rotation) {
    Texture2D texture = original_texture(lean_string_cstr(name));
    if (!texture.id || sx >= (uint32_t)texture.width || sy >= (uint32_t)texture.height) {
        return lean_io_result_mk_ok(lean_box(0));
    }
    if (!sw) sw = (uint32_t)texture.width - sx;
    if (!sh) sh = (uint32_t)texture.height - sy;
    if (sw > (uint32_t)texture.width - sx || sh > (uint32_t)texture.height - sy || !width || !height) {
        return lean_io_result_mk_ok(lean_box(0));
    }
    float w = width;
    float h = height;
    if (flags & 1) {
        float scale = fminf(w / sw, h / sh);
        w = sw * scale;
        h = sh * scale;
    }
    Rectangle source = { (float)sx, (float)sy, (flags & 2) ? -(float)sw : (float)sw, (float)sh };
    Rectangle dest = { (float)(int32_t)x + width / 2.0f, (float)(int32_t)y + height / 2.0f, w, h };
    if (flags & 4) BeginBlendMode(BLEND_ADDITIVE);
    bool queen_slime = (flags & 24) && begin_queen_slime_shader((flags & 16) != 0);
    DrawTexturePro(texture, source, dest, (Vector2){w / 2, h / 2}, (float)rotation, GetColor(color));
    if (queen_slime) EndShaderMode();
    if (flags & 4) EndBlendMode();
    return lean_io_result_mk_ok(lean_box(1));
}

lean_obj_res terria_original_posed_sprite_region(b_lean_obj_arg name, uint32_t sx, uint32_t sy, uint32_t width,
        uint32_t height, uint32_t color, double x, double y, double pivot_x, double pivot_y,
        double scale, double rotation, uint8_t mirror) {
    Texture2D texture = original_texture(lean_string_cstr(name));
    if (texture.id && width && height && sx < (uint32_t)texture.width && sy < (uint32_t)texture.height &&
            width <= (uint32_t)texture.width - sx && height <= (uint32_t)texture.height - sy) {
        float source_width = (float)width;
        Rectangle source = { (float)sx, (float)sy, mirror ? -source_width : source_width, (float)height };
        Rectangle dest = { (float)x, (float)y, (float)(width * scale), (float)(height * scale) };
        Vector2 origin = { (float)(pivot_x * scale), (float)(pivot_y * scale) };
        DrawTexturePro(texture, source, dest, origin, (float)(rotation * RAD2DEG), GetColor(color));
    }
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_original_posed_sprite(b_lean_obj_arg name, uint32_t sy, uint32_t width,
        uint32_t height, uint32_t color, double x, double y, double pivot_x, double pivot_y,
        double scale, double rotation, uint8_t mirror) {
    return terria_original_posed_sprite_region(name, 0, sy, width, height, color, x, y,
        pivot_x, pivot_y, scale, rotation, mirror);
}

static uint32_t float_word(float value) {
    uint32_t word;
    memcpy(&word, &value, sizeof(word));
    return word;
}

static lean_obj_res original_font_rows(TerrariaFont *font) {
    if (!font->glyph) return lean_io_result_mk_ok(lean_mk_string(""));

    size_t glyph_count = 0;
    for (unsigned i = 0; i <= UINT16_MAX; ++i) glyph_count += font->glyph[i].present;
    size_t capacity = 64 + 24 * font->page_count + 160 * glyph_count;
    char *rows = malloc(capacity);
    if (!rows) return lean_io_result_mk_ok(lean_mk_string(""));
    size_t used = snprintf(rows, capacity, "F %u %u %u\nP", float_word(font->spacing),
        font->line_spacing, font->default_char);
    for (unsigned i = 0; i < font->page_count; ++i)
        used += snprintf(rows + used, capacity - used, " %u,%u", font->pages[i].width, font->pages[i].height);
    for (unsigned i = 0; i <= UINT16_MAX; ++i) {
        const TerrariaGlyph *glyph = &font->glyph[i];
        if (!glyph->present) continue;
        used += snprintf(rows + used, capacity - used, "\nG %u %u %d %d %d %d %d %d %d %d %u %u %u",
            i, glyph->page, (int)glyph->source.x, (int)glyph->source.y,
            (int)glyph->source.width, (int)glyph->source.height, (int)glyph->offset_x, (int)glyph->offset_y,
            glyph->crop_width, glyph->crop_height, float_word(glyph->left), float_word(glyph->width),
            float_word(glyph->right));
    }
    lean_obj_res result = lean_mk_string(rows);
    free(rows);
    return lean_io_result_mk_ok(result);
}

lean_obj_res terria_original_chat_font(uint8_t item_stack) {
    TerrariaFont *font = item_stack ? &item_stack_font : &terraria_font;
    if (item_stack && !font->glyph) {
        size_t bytes = 0;
        unsigned char *payload = read_original_xnb("Fonts", "Item_Stack", &bytes);
        load_terraria_font(font, payload, bytes, false);
    }
    return original_font_rows(font);
}

lean_obj_res terria_original_combat_font(uint8_t critical) {
    TerrariaFont *font = critical ? &combat_crit_font : &combat_text_font;
    if (!font->glyph) {
        size_t bytes = 0;
        /* Combat_Crit expands to 38,811,262 bytes in desktop1.4.5.8. Its
           complete multilingual atlas exceeds the general 32MiB asset cap. */
        unsigned char *payload = read_original_xnb_bytes("Fonts", critical ? "Combat_Crit" : "Combat_Text",
            &bytes, 128 * 1024 * 1024);
        load_terraria_font(font, payload, bytes, false);
    }
    return original_font_rows(font);
}

static Texture2D original_chat_texture(const char *name) {
    const char mouse_page[] = "Fonts/Mouse_Text.page";
    const char item_page[] = "Fonts/Item_Stack.page";
    const char combat_page[] = "Fonts/Combat_Text.page";
    const char critical_page[] = "Fonts/Combat_Crit.page";
    if (!strncmp(name, "Images/", 7)) return original_texture(name + 7);
    if (!strncmp(name, mouse_page, sizeof(mouse_page) - 1))
        return terraria_font_texture(&terraria_font, (unsigned)strtoul(name + sizeof(mouse_page) - 1, NULL, 10));
    if (!strncmp(name, item_page, sizeof(item_page) - 1))
        return terraria_font_texture(&item_stack_font, (unsigned)strtoul(name + sizeof(item_page) - 1, NULL, 10));
    if (!strncmp(name, combat_page, sizeof(combat_page) - 1))
        return terraria_font_texture(&combat_text_font, (unsigned)strtoul(name + sizeof(combat_page) - 1, NULL, 10));
    if (!strncmp(name, critical_page, sizeof(critical_page) - 1))
        return terraria_font_texture(&combat_crit_font, (unsigned)strtoul(name + sizeof(critical_page) - 1, NULL, 10));
    return (Texture2D){0};
}

lean_obj_res terria_original_chat_sprite(b_lean_obj_arg name, float sx, float sy, float sw, float sh,
        float x, float y, float w, float h, float ox, float oy, float sine, float cosine,
        uint32_t rgba, uint32_t effects) {
    Texture2D texture = original_chat_texture(lean_string_cstr(name));
    if (texture.id && sw > 0 && sh > 0 && w > 0 && h > 0) {
        float left = -ox * w, right = left + w, top = -oy * h, bottom = top + h;
        float u0 = sx, u1 = sx + sw, v0 = sy, v1 = sy + sh;
        if (effects & 1) { u0 = sx + sw; u1 = sx; }
        if (effects & 2) { v0 = sy + sh; v1 = sy; }
        rlSetTexture(texture.id);
        rlBegin(RL_QUADS);
        rlColor4ub(rgba, rgba >> 8, rgba >> 16, rgba >> 24);
        rlNormal3f(0.0f, 0.0f, 1.0f);
        rlTexCoord2f(u0, v0); rlVertex2f(x + left * cosine - top * sine, y + left * sine + top * cosine);
        rlTexCoord2f(u0, v1); rlVertex2f(x + left * cosine - bottom * sine, y + left * sine + bottom * cosine);
        rlTexCoord2f(u1, v1); rlVertex2f(x + right * cosine - bottom * sine, y + right * sine + bottom * cosine);
        rlTexCoord2f(u1, v0); rlVertex2f(x + right * cosine - top * sine, y + right * sine + top * cosine);
        rlEnd();
        rlSetTexture(0);
    }
    return lean_io_result_mk_ok(lean_box(0));
}

static bool original_assembly_path(char path[PATH_MAX]) {
    int length = snprintf(path, PATH_MAX, "%s/../Terraria.exe", original_content);
    return original_content[0] && length >= 0 && length < PATH_MAX;
}

lean_obj_res terria_original_assembly_path(void) {
    char path[PATH_MAX];
    return lean_io_result_mk_ok(lean_mk_string(original_assembly_path(path) ? path : ""));
}

/* Read the ordered English death-text category from the local Terraria assembly. */
lean_obj_res terria_original_death_templates(void) {
    char path[PATH_MAX];
    if (!original_assembly_path(path)) return lean_io_result_mk_ok(lean_mk_string(""));
    int size = 0;
    unsigned char *assembly = LoadFileData(path, &size);
    if (!assembly) return lean_io_result_mk_ok(lean_mk_string(""));
    static const char english_marker[] = "\"LaunchFromSteam\": \"Please launch the game from your Steam client.\"";
    static const char category[] = "\n\t\"DeathTextGeneric\": {\n";
    static const char end[] = "\n\t},\n\t\"DeathSource\"";
    static const char english_npc[] = "\"NPC\": \"{0} by {1}.\"";
    const char *begin = (const char *)assembly;
    const char *limit = begin + size;
    const char *english = memmem(begin, size, english_marker, strlen(english_marker));
    const char *first = english ? memmem(english, limit - english, category, strlen(category)) : NULL;
    lean_object *result = NULL;
    if (first && first - english < 1024 * 128) {
        first += strlen(category);
        const char *last = memmem(first, limit - first, end, strlen(end));
        if (last && last - first < 1024 * 16 &&
                memmem(last, limit - last < 256 ? limit - last : 256, english_npc, strlen(english_npc))) {
            result = lean_mk_string_from_bytes(first, last - first);
        }
    }
    UnloadFileData(assembly);
    return lean_io_result_mk_ok(result ? result : lean_mk_string(""));
}

lean_obj_res terria_original_local_death_date(void) {
    static const char *months[] = {"January", "February", "March", "April", "May", "June",
        "July", "August", "September", "October", "November", "December"};
    time_t current = time(NULL);
    struct tm local;
    if (!localtime_r(&current, &local)) return lean_io_result_mk_ok(lean_mk_string(""));
    char text[64];
    snprintf(text, sizeof(text), "%s %d, %03d", months[local.tm_mon], local.tm_mday, 1900 + local.tm_year);
    return lean_io_result_mk_ok(lean_mk_string(text));
}

lean_obj_res terria_begin_liquid_layer(uint8_t foreground, uint32_t width, uint32_t height) {
    RenderTexture2D *layer = &original_liquid_layers[foreground != 0];
    if (!layer->id) *layer = LoadRenderTexture((int)width, (int)height);
    if (!layer->id) {
        return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string("Could not allocate a liquid render target.")));
    }
    BeginTextureMode(*layer);
    ClearBackground(BLANK);
    BeginBlendMode(BLEND_ALPHA_PREMULTIPLY);
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_end_liquid_layer(void) {
    EndBlendMode();
    EndTextureMode();
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_draw_liquid_layer(uint8_t foreground, uint32_t x, uint32_t y) {
    Texture2D texture = original_liquid_layers[foreground != 0].texture;
    if (texture.id) {
        BeginBlendMode(BLEND_ALPHA_PREMULTIPLY);
        DrawTextureRec(texture, (Rectangle){0, 0, texture.width, -texture.height},
            (Vector2){(float)(int32_t)x, (float)(int32_t)y}, WHITE);
        EndBlendMode();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_premultiplied_alpha(uint8_t enabled) {
    if (enabled) BeginBlendMode(BLEND_ALPHA_PREMULTIPLY);
    else EndBlendMode();
    return lean_io_result_mk_ok(lean_box(0));
}

static void liquid_vertex(float x, float y, float u, float v, uint32_t rgba) {
    Color color = GetColor(rgba);
    rlColor4ub(color.r, color.g, color.b, color.a);
    rlTexCoord2f(u, v);
    rlVertex2f(x, y);
}

static void draw_liquid_quad(Texture2D texture, uint32_t sx, uint32_t sy, uint32_t width, uint32_t height,
        float left, float top, uint32_t tl, uint32_t bl, uint32_t br, uint32_t tr, bool flip_x) {
    if (!texture.id || !width || !height) return;
    float right = left + width, bottom = top + height;
    // TileBatch permits source rectangles beyond the image; the sampler clamps them.
    float u0 = (float)sx / texture.width, v0 = (float)sy / texture.height;
    float u1 = (float)(sx + width) / texture.width, v1 = (float)(sy + height) / texture.height;
    if (flip_x) {
        float saved = u0;
        u0 = u1;
        u1 = saved;
    }
    rlCheckRenderBatchLimit(4);
    rlSetTexture(texture.id);
    rlBegin(RL_QUADS);
    liquid_vertex(left, top, u0, v0, tl);
    liquid_vertex(left, bottom, u0, v1, bl);
    liquid_vertex(right, bottom, u1, v1, br);
    liquid_vertex(right, top, u1, v0, tr);
    rlEnd();
    rlSetTexture(0);
}

lean_obj_res terria_liquid_quad(b_lean_obj_arg name, uint32_t sx, uint32_t sy, uint32_t width, uint32_t height,
        double x, double y, uint32_t tl, uint32_t bl, uint32_t br, uint32_t tr) {
    draw_liquid_quad(original_texture(lean_string_cstr(name)), sx, sy, width, height,
        (float)x, (float)y, tl, bl, br, tr, false);
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_waterfall_quad(b_lean_obj_arg name, uint32_t sx, uint32_t sy, uint32_t width, uint32_t height,
        double x, double y, uint32_t tl, uint32_t bl, uint32_t br, uint32_t tr, uint8_t flip_x) {
    draw_liquid_quad(original_texture(lean_string_cstr(name)), sx, sy, width, height,
        (float)x, (float)y, tl, bl, br, tr, flip_x != 0);
    return lean_io_result_mk_ok(lean_box(0));
}

static bool init_magic_missile_shader(void) {
    if (!magic_missile_shader_tried) {
        static const char fragment[] =
            "#version 330\n"
            "in vec2 fragTexCoord; in vec4 fragColor; out vec4 finalColor;\n"
            "uniform sampler2D texture0, veins, noiseMap; uniform float clockPhase, flamePhase;\n"
            "void main() {\n"
            "  vec2 uv = fragTexCoord;\n"
            "  float edge = 1.0 - abs(uv.y * 2.0 - 1.0);\n"
            "  float grain = texture(noiseMap, vec2(fract(uv.x - clockPhase), uv.y)).r;\n"
            "  float strands = texture(veins, vec2(fract(uv.x + clockPhase * 1.6), uv.y)).r;\n"
            "  vec3 palette = texture(texture0, vec2(0.5, uv.y)).rgb;\n"
            "  if (flamePhase >= 0.0) {\n"
            "    float dark = flamePhase;\n"
            "    float sweep = clamp((uv.x + 0.1 * dark) / (0.7 - 0.1 * dark), 0.0, 1.0);\n"
            "    vec3 core = mix(vec3(1.0), vec3(1.0, 0.647, 0.0), 0.5 * dark);\n"
            "    vec3 fire = mix(core, vec3(1.0, 0.1, 0.02), sweep);\n"
            "    float glow = pow(max(0.0, edge), 3.4) * (0.2 + grain * 0.34 + strands * 0.53);\n"
            "    finalColor = vec4(mix(palette, fire, 0.55), glow * (1.0 + dark * 0.35) *\n"
            "      (1.0 - uv.x) * fragColor.a);\n"
            "    return;\n"
            "  }\n"
            "  vec3 tone = mix(vec3(1.0), vec3(0.55, 0.18, 1.0), smoothstep(0.0, 0.7, uv.x));\n"
            "  float glow = pow(max(0.0, edge), 3.8) * (0.21 + grain * 0.21 + strands * 0.74);\n"
            "  finalColor = vec4(mix(palette, tone, 0.45), glow * (1.0 - uv.x) * fragColor.a);\n"
            "}\n";
        magic_missile_shader_tried = true;
        magic_missile_shader = LoadShaderFromMemory(NULL, fragment);
        missile_veins_loc = GetShaderLocation(magic_missile_shader, "veins");
        missile_noise_loc = GetShaderLocation(magic_missile_shader, "noiseMap");
        missile_clock_loc = GetShaderLocation(magic_missile_shader, "clockPhase");
        missile_flame_loc = GetShaderLocation(magic_missile_shader, "flamePhase");
    }
    return magic_missile_shader.id && missile_veins_loc >= 0 && missile_noise_loc >= 0 &&
        missile_clock_loc >= 0 && missile_flame_loc >= 0;
}

static void missile_trail_vertex(const double *point, float progress, float side, float flame_phase) {
    float half_width;
    if (flame_phase >= 0.0f) {
        float taper = 1.0f - fminf(1.0f, progress / (0.06f + 0.01f * flame_phase));
        float wide = 24.0f + 16.0f * flame_phase;
        half_width = (wide + (8.0f - wide) * progress) * (1.0f - taper * taper);
    } else {
        half_width = (26.0f + 6.0f * fminf(1.0f, progress / 0.2f)) * fminf(1.0f, progress / 0.07f);
    }
    float offset = half_width * 1.5f * (side == 0.0f ? -1.0f : 1.0f);
    float rotation = (float)point[2];
    rlTexCoord2f(progress, side);
    rlVertex2f((float)point[0] - sinf(rotation) * offset, (float)point[1] + cosf(rotation) * offset);
}

static lean_obj_res draw_missile_trail(b_lean_obj_arg points, double clock, double opacity, float flame_phase) {
    size_t count = lean_sarray_size(points) / 3;
    if (count < 2 || count > 31 || !original_content[0]) return lean_io_result_mk_ok(lean_box(0));
    bool flame = flame_phase >= 0.0f;
    Texture2D palette = original_texture(flame ? "Extra_191" : "Extra_192");
    Texture2D veins = original_texture(flame ? "Extra_189" : "Extra_194");
    Texture2D noise = original_texture(flame ? "Extra_190" : "Extra_193");
    if (!palette.id || !veins.id || !noise.id || !init_magic_missile_shader()) {
        return lean_io_result_mk_ok(lean_box(0));
    }
    const double *trail = lean_float_array_cptr(points);
    float phase = (float)clock;
    BeginBlendMode(BLEND_ADDITIVE);
    BeginShaderMode(magic_missile_shader);
    SetShaderValueTexture(magic_missile_shader, missile_veins_loc, veins);
    SetShaderValueTexture(magic_missile_shader, missile_noise_loc, noise);
    SetShaderValue(magic_missile_shader, missile_clock_loc, &phase, SHADER_UNIFORM_FLOAT);
    SetShaderValue(magic_missile_shader, missile_flame_loc, &flame_phase, SHADER_UNIFORM_FLOAT);
    rlDisableBackfaceCulling();
    rlSetTexture(palette.id);
    rlBegin(RL_QUADS);
    rlColor4ub(255, 255, 255, (unsigned char)(opacity * 255.0));
    for (size_t i = 1; i < count; ++i) {
        float start = (float)(i - 1) / (float)(count - 1);
        float end = (float)i / (float)(count - 1);
        const double *head = trail + (i - 1) * 3;
        const double *tail = trail + i * 3;
        missile_trail_vertex(head, start, 0.0f, flame_phase);
        missile_trail_vertex(head, start, 1.0f, flame_phase);
        missile_trail_vertex(tail, end, 1.0f, flame_phase);
        missile_trail_vertex(tail, end, 0.0f, flame_phase);
    }
    rlEnd();
    rlDrawRenderBatchActive();
    rlEnableBackfaceCulling();
    rlSetTexture(0);
    EndShaderMode();
    EndBlendMode();
    return lean_io_result_mk_ok(lean_box(1));
}

lean_obj_res terria_magic_missile_trail(b_lean_obj_arg points, double clock, double opacity) {
    return draw_missile_trail(points, clock, opacity, -1.0f);
}

lean_obj_res terria_flamelash_trail(b_lean_obj_arg points, double clock, double opacity, double empowerment) {
    return draw_missile_trail(points, clock, opacity, (float)fmin(1.0, fmax(0.0, empowerment)));
}

lean_obj_res terria_sound(uint32_t kind) {
    if (kind < sound_count && IsAudioDeviceReady() && IsSoundValid(sounds[kind])) {
        PlaySound(sounds[kind]);
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/* Each accepted Coin/MaxMana event creates an overlapping native instance.
   Completed aliases are retired before their source assets are unloaded. */
static void play_original_alias(Sound source, float volume, float pan, float pitch) {
    if (!IsAudioDeviceReady() || !IsSoundValid(source)) return;
    for (size_t i = 0; i < original_sound_instance_count;) {
        if (IsSoundPlaying(original_sound_instances[i])) { ++i; continue; }
        UnloadSoundAlias(original_sound_instances[i]);
        original_sound_instances[i] = original_sound_instances[--original_sound_instance_count];
    }
    Sound *grown = realloc(original_sound_instances,
        (original_sound_instance_count + 1) * sizeof *grown);
    if (!grown) return;
    original_sound_instances = grown;
    Sound instance = LoadSoundAlias(source);
    if (IsSoundValid(instance)) {
        SetSoundVolume(instance, volume);
        SetSoundPan(instance, pan);
        SetSoundPitch(instance, exp2f(pitch));
        PlaySound(instance);
        original_sound_instances[original_sound_instance_count++] = instance;
    }
}

/* Vanilla event25 is MaxMana, not the project's numbered sound slot25. */
lean_obj_res terria_original_mana_refill_sound(void) {
    play_original_alias(original_mana_refill, 1.0f, 0.0f, 0.0f);
    return lean_io_result_mk_ok(lean_box(0));
}

lean_obj_res terria_original_audio_supported(void) {
    return lean_io_result_mk_ok(lean_box(IsAudioDeviceReady()));
}

static void replace_original_voice(Sound *previous, Sound source,
        float volume, float pan, float pitch) {
    if (IsSoundValid(*previous)) {
        StopSound(*previous);
        UnloadSoundAlias(*previous);
        *previous = (Sound){0};
    }
    if (!IsAudioDeviceReady() || !IsSoundValid(source)) return;
    *previous = LoadSoundAlias(source);
    if (IsSoundValid(*previous)) {
        SetSoundVolume(*previous, volume);
        SetSoundPan(*previous, pan);
        SetSoundPitch(*previous, exp2f(pitch));
        PlaySound(*previous);
    }
}

/* Parameters and asset choices are already resolved by the player RNG.
   Grab stops its previous instance; coin calls overlap even on the same asset. */
lean_obj_res terria_original_tile_hit_sound(uint32_t type, uint32_t style,
        uint32_t variation, float volume, float pan, float pitch) {
    if (volume >= 0.0f && IsAudioDeviceReady()) {
        if (type == 38 && variation < 5) {
            play_original_alias(original_coin[variation], volume, pan, pitch);
        } else if (type == 7 && variation == 0) {
            replace_original_voice(&original_grab_instance, original_pickup_grab, volume, pan, pitch);
        } else if (type == 2 && style >= 48 && style <= 50 && variation == style) {
            replace_original_voice(&original_snow_ice_instances[style - 48],
                original_snow_ice[style - 48], volume, pan, pitch);
        } else if (type == 1 && variation < 3) {
            /* LegacySoundPlayer stops the previous instance of the selected variation,
               whatever its state, and starts a fresh one. */
            replace_original_voice(&original_player_hit_instances[variation],
                original_player_hit[variation], volume, pan, pitch);
        } else if (type == 20 && variation < 3) {
            replace_original_voice(&original_female_hit_instances[variation],
                original_female_hit[variation], volume, pan, pitch);
        } else if (type == 14 && style == 1 && variation < 3) {
            play_original_alias(original_zombie[variation], volume, pan, pitch);
        } else if (type == 5 && style == 1) {
            replace_original_voice(&original_player_killed_instance, sounds[player_death_sound], volume, pan, pitch);
        } else if (type == 3 && style >= 1 && style < npc_sound_styles) {
            /* NPCHit stops the previous instance of its style (LegacySoundPlayer IL_0815..0844). */
            if (!original_npc_hit_tried[style]) {
                char name[24];
                snprintf(name, sizeof name, "NPC_Hit_%u", style);
                original_npc_hit[style] = load_original_sound(name);
                original_npc_hit_tried[style] = true;
            }
            replace_original_voice(&original_npc_hit_instance[style], original_npc_hit[style], volume, pan, pitch);
        } else if (type == 4 && style >= 1 && style < npc_sound_styles) {
            /* NPCKilled never stops an earlier instance. */
            if (!original_npc_killed_tried[style]) {
                char name[24];
                snprintf(name, sizeof name, "NPC_Killed_%u", style);
                original_npc_killed[style] = load_original_sound(name);
                original_npc_killed_tried[style] = true;
            }
            play_original_alias(original_npc_killed[style], volume, pan, pitch);
        } else if (type == 42 && style == 317 && variation == 0) {
            /* The default voice never stops an earlier instance: voices overlap. */
            play_original_alias(original_player_hit_default, volume, pan, pitch);
        } else {
            Sound voice = {0};
            if (type == 0 && variation < 3) voice = original_tile_dig[variation];
            else if (type == 21 && variation < 3) voice = original_tink[variation];
            else if (type == 2 && style == 1 && variation < 3) voice = original_item_swing[variation];
            else return lean_io_result_mk_error(lean_mk_io_user_error(
                lean_mk_string("Sound is outside the checked Dig/Tink/swing/pickup/snow-ice backend.")));
            if (IsSoundValid(voice)) {
                StopSound(voice);
                SetSoundVolume(voice, volume);
                SetSoundPan(voice, pan);
                SetSoundPitch(voice, exp2f(pitch));
                PlaySound(voice);
            }
        }
    }
    return lean_io_result_mk_ok(lean_box(0));
}

static bool bucket_splash_ready(void) {
    return IsAudioDeviceReady() && IsSoundValid(sounds[bucket_splash_sound]) &&
        GetMasterVolume() > 0.0f && !IsSoundPlaying(sounds[bucket_splash_sound]);
}

lean_obj_res terria_bucket_splash_ready(void) {
    return lean_io_result_mk_ok(lean_box(bucket_splash_ready()));
}

lean_obj_res terria_bucket_splash(double pitch, double volume, double pan) {
    if (bucket_splash_ready() && volume > 0.0) {
        Sound splash = sounds[bucket_splash_sound];
        SetSoundPitch(splash, exp2f((float)pitch));
        SetSoundVolume(splash, (float)volume);
        SetSoundPan(splash, (float)pan);
        PlaySound(splash);
    }
    return lean_io_result_mk_ok(lean_box(0));
}
