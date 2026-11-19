/* Terraria's DynamicSpriteFont XNB. Atlas pages are uploaded on first use. */

typedef struct {
    Rectangle source;
    float offset_x, offset_y;
    int32_t crop_width, crop_height;
    float left, width, right;
    unsigned page;
    bool present;
} TerrariaGlyph;

typedef struct {
    Texture2D texture;
    const unsigned char *blocks;
    unsigned width, height;
} TerrariaFontPage;

typedef struct {
    unsigned char *payload;
    TerrariaGlyph *glyph;
    TerrariaFontPage *pages;
    unsigned page_count, default_char;
    unsigned line_spacing;
    float spacing;
} TerrariaFont;

static TerrariaFont terraria_font, death_font, item_stack_font;

static int sprite_font_char(const unsigned char **data, size_t *bytes) {
    if (!*bytes) return -1;
    unsigned char first = **data;
    unsigned length = first < 0x80 ? 1 : first < 0xe0 ? 2 : first < 0xf0 ? 3 : 4;
    if (length > *bytes || first >= 0xf8 || (first >= 0x80 && first < 0xc2)) return -1;
    int value = first & (0x7f >> (length == 1 ? 0 : length));
    for (unsigned i = 1; i < length; ++i) {
        unsigned char next = (*data)[i];
        if ((next & 0xc0) != 0x80) return -1;
        value = (value << 6) | (next & 0x3f);
    }
    *data += length;
    *bytes -= length;
    return value;
}

static float font_float(const unsigned char *data) {
    uint32_t word = read_le32(data);
    float result;
    memcpy(&result, &word, sizeof(result));
    return result;
}

static Color font_rgb565(unsigned word) {
    return (Color){ (unsigned char)(((word >> 11) & 31) * 255 / 31),
        (unsigned char)(((word >> 5) & 63) * 255 / 63), (unsigned char)((word & 31) * 255 / 31), 255 };
}

static Texture2D font_dxt3_texture(const unsigned char *blocks, unsigned width, unsigned height) {
    Color *pixels = malloc((size_t)width * height * sizeof(*pixels));
    if (!pixels) return (Texture2D){0};
    for (unsigned y = 0; y < height; y += 4) {
        for (unsigned x = 0; x < width; x += 4, blocks += 16) {
            Color palette[4] = { font_rgb565(blocks[8] | ((unsigned)blocks[9] << 8)),
                font_rgb565(blocks[10] | ((unsigned)blocks[11] << 8)) };
            for (unsigned index = 2; index < 4; ++index) {
                Color a = palette[index - 2], b = palette[3 - index];
                palette[index] = (Color){ (2 * a.r + b.r) / 3, (2 * a.g + b.g) / 3, (2 * a.b + b.b) / 3, 255 };
            }
            uint32_t indices = read_le32(blocks + 12);
            for (unsigned index = 0; index < 16; ++index) {
                Color pixel = palette[(indices >> (2 * index)) & 3];
                pixel.a = ((blocks[index / 2] >> (4 * (index % 2))) & 15) * 17;
                pixels[(y + index / 4) * width + x + index % 4] = pixel;
            }
        }
    }
    Image atlas = { .data = pixels, .width = (int)width, .height = (int)height,
        .mipmaps = 1, .format = PIXELFORMAT_UNCOMPRESSED_R8G8B8A8 };
    Texture2D texture = LoadTextureFromImage(atlas);
    free(pixels);
    if (texture.id) SetTextureFilter(texture, TEXTURE_FILTER_POINT);
    return texture;
}

static void unload_terraria_font(TerrariaFont *font) {
    if (font->pages) {
        for (unsigned i = 0; i < font->page_count; ++i)
            if (font->pages[i].texture.id) UnloadTexture(font->pages[i].texture);
    }
    free(font->glyph);
    free(font->pages);
    free(font->payload);
    *font = (TerrariaFont){0};
}

static bool read_terraria_font_page(TerrariaFont *font, unsigned page,
        const unsigned char **input, size_t *remaining) {
    const unsigned char *data = *input;
    size_t bytes = *remaining;
    if (read_xnb_int(&data, &bytes) != 2 || bytes < 20 || read_le32(data) != 5) return false;
    unsigned width = read_le32(data + 4), height = read_le32(data + 8);
    unsigned image_bytes = read_le32(data + 16);
    if (!width || !height || width > 1024 || height > 1024 || width % 4 || height % 4 ||
            image_bytes != width * height || image_bytes > bytes - 20 || read_le32(data + 12) != 1) return false;
    font->pages[page] = (TerrariaFontPage){ .blocks = data + 20, .width = width, .height = height };
    data += 20 + image_bytes;
    bytes -= 20 + image_bytes;
    if (read_xnb_int(&data, &bytes) != 3 || bytes < 4) return false;
    unsigned count = read_le32(data);
    if (!count || count > 512 || count * 16 > bytes - 4) return false;
    const unsigned char *rectangles = data + 4;
    data += 4 + count * 16;
    bytes -= 4 + count * 16;
    if (read_xnb_int(&data, &bytes) != 3 || bytes < 4 || read_le32(data) != count || count * 16 > bytes - 4) return false;
    const unsigned char *cropping = data + 4;
    data += 4 + count * 16;
    bytes -= 4 + count * 16;
    if (read_xnb_int(&data, &bytes) != 5 || bytes < 4 || read_le32(data) != count) return false;
    data += 4;
    bytes -= 4;
    int letters[512];
    for (unsigned i = 0; i < count; ++i) {
        letters[i] = sprite_font_char(&data, &bytes);
        if (letters[i] < 0 || letters[i] > UINT16_MAX) return false;
    }
    if (read_xnb_int(&data, &bytes) != 7 || bytes < 4 || read_le32(data) != count || count * 12 > bytes - 4) return false;
    data += 4;
    bytes -= 4;
    for (unsigned i = 0; i < count; ++i) {
        const unsigned char *rect = rectangles + 16 * i;
        unsigned x = read_le32(rect), y = read_le32(rect + 4), w = read_le32(rect + 8), h = read_le32(rect + 12);
        if (x > width || y > height || w > width - x || h > height - y) continue;
        float left = font_float(data + 12 * i), center = font_float(data + 12 * i + 4), right = font_float(data + 12 * i + 8);
        if (!(fabsf(left) <= 128 && center >= 0 && center <= 128 && fabsf(right) <= 128)) continue;
        const unsigned char *crop = cropping + 16 * i;
        font->glyph[letters[i]] = (TerrariaGlyph){
            .source = { x, y, w, h },
            .offset_x = (int32_t)read_le32(crop), .offset_y = (int32_t)read_le32(crop + 4),
            .crop_width = (int32_t)read_le32(crop + 8), .crop_height = (int32_t)read_le32(crop + 12),
            .left = left, .width = center, .right = right, .page = page, .present = true };
    }
    *input = data + count * 12;
    *remaining = bytes - count * 12;
    return true;
}

/* Takes ownership of payload. A failed optional upgrade preserves the font already loaded. */
static bool load_terraria_font(TerrariaFont *font, unsigned char *payload, size_t bytes, bool first_page) {
    static const char reader_name[] = "ReLogic.Graphics.DynamicSpriteFontReader,";
    TerrariaFont loaded = { .payload = payload };
    const unsigned char *data = payload;
    if (!data || read_xnb_int(&data, &bytes) != 8) goto invalid;
    for (unsigned index = 0; index < 8; ++index) {
        unsigned length = read_xnb_int(&data, &bytes);
        if (length > bytes || bytes - length < 4) goto invalid;
        if (!index && (length < sizeof(reader_name) - 1 || memcmp(data, reader_name, sizeof(reader_name) - 1))) goto invalid;
        data += length + 4;
        bytes -= length + 4;
    }
    if (read_xnb_int(&data, &bytes) != 0 || read_xnb_int(&data, &bytes) != 1 || bytes < 8) goto invalid;
    float spacing = font_float(data);
    unsigned line_spacing = read_le32(data + 4);
    data += 8;
    bytes -= 8;
    int default_char = sprite_font_char(&data, &bytes);
    if (default_char < 0 || default_char > UINT16_MAX || bytes < 4 ||
            line_spacing < 10 || line_spacing > 100 || !(spacing >= -4 && spacing <= 8)) goto invalid;
    unsigned pages = read_le32(data);
    if (!pages || pages > 512) goto invalid;
    data += 4;
    bytes -= 4;
    loaded.page_count = first_page ? 1 : pages;
    loaded.pages = calloc(loaded.page_count, sizeof(*loaded.pages));
    loaded.glyph = calloc(UINT16_MAX + 1u, sizeof(*loaded.glyph));
    if (!loaded.pages || !loaded.glyph) goto invalid;
    for (unsigned i = 0; i < loaded.page_count; ++i)
        if (!read_terraria_font_page(&loaded, i, &data, &bytes)) goto invalid;
    if (!loaded.glyph['A'].present || !loaded.glyph[' '].present || !loaded.glyph[default_char].present) goto invalid;
    loaded.default_char = (unsigned)default_char;
    loaded.line_spacing = line_spacing;
    loaded.spacing = spacing;
    unload_terraria_font(font);
    *font = loaded;
    return true;

invalid:
    unload_terraria_font(&loaded);
    return false;
}

static unsigned next_font_unit(const char **text, unsigned *second) {
    if (*second) {
        unsigned letter = *second;
        *second = 0;
        return letter;
    }
    int bytes;
    unsigned letter = (unsigned)GetCodepointNext(*text, &bytes);
    *text += bytes;
    if (letter <= UINT16_MAX) return letter;
    letter -= 0x10000;
    *second = 0xdc00 + (letter & 0x3ff);
    return 0xd800 + (letter >> 10);
}

static TerrariaGlyph terraria_glyph(const TerrariaFont *font, unsigned letter) {
    return font->glyph[font->glyph[letter].present ? letter : font->default_char];
}

static Texture2D terraria_font_texture(TerrariaFont *font, unsigned index) {
    if (index >= font->page_count) return (Texture2D){0};
    TerrariaFontPage *page = &font->pages[index];
    if (!page->texture.id) page->texture = font_dxt3_texture(page->blocks, page->width, page->height);
    return page->texture;
}

static float terraria_font_width(const TerrariaFont *font, const char *text) {
    if (!font->glyph) return 0;
    float line_width = 0, width = 0, right = 0;
    bool first = true;
    unsigned second = 0;
    while (*text || second) {
        unsigned letter = next_font_unit(&text, &second);
        if (letter == '\n') {
            width = fmaxf(width, line_width + fmaxf(0, right));
            line_width = right = 0;
            first = true;
            continue;
        }
        TerrariaGlyph glyph = terraria_glyph(font, letter);
        if (!first) line_width += font->spacing + right;
        line_width += (first ? fmaxf(0, glyph.left) : glyph.left) + glyph.width;
        right = glyph.right;
        first = false;
    }
    return fmaxf(width, line_width + fmaxf(0, right));
}

static void draw_terraria_font(TerrariaFont *font, const char *text, float x, float y, float scale, Color color) {
    if (!font->glyph) return;
    float pen_x = x, pen_y = y;
    bool first = true;
    unsigned second = 0;
    while (*text || second) {
        unsigned letter = next_font_unit(&text, &second);
        if (letter == '\n') {
            pen_x = x;
            pen_y += font->line_spacing * scale;
            first = true;
            continue;
        }
        TerrariaGlyph glyph = terraria_glyph(font, letter);
        pen_x += (first ? (glyph.left > 0 ? glyph.left : 0) : font->spacing + glyph.left) * scale;
        if (glyph.source.width && glyph.source.height) {
            Texture2D texture = terraria_font_texture(font, glyph.page);
            Rectangle destination = { pen_x + glyph.offset_x * scale, pen_y + glyph.offset_y * scale,
                glyph.source.width * scale, glyph.source.height * scale };
            if (texture.id) DrawTexturePro(texture, glyph.source, destination, (Vector2){0}, 0, color);
        }
        pen_x += (glyph.width + glyph.right) * scale;
        first = false;
    }
}
