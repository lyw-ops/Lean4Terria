#include <CommonCrypto/CommonCryptor.h>
#include <lean/lean.h>

static lean_obj_res crypto_error(const char *message) {
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(message)));
}

lean_obj_res terria_player_crypt(uint8_t encrypt, b_lean_obj_arg key, b_lean_obj_arg input) {
    size_t input_size = lean_sarray_size(input);
    if (lean_sarray_size(key) != kCCKeySizeAES128 ||
        input_size > SIZE_MAX - kCCBlockSizeAES128) {
        return crypto_error("Invalid Terraria character encryption parameters.");
    }
    size_t capacity = input_size + kCCBlockSizeAES128;
    lean_object *output = lean_alloc_sarray(1, 0, capacity);
    const uint8_t *key_bytes = lean_sarray_cptr(key);
    size_t size = 0;
    CCCryptorStatus result = CCCrypt(encrypt ? kCCEncrypt : kCCDecrypt,
        kCCAlgorithmAES, kCCOptionPKCS7Padding, key_bytes, kCCKeySizeAES128,
        key_bytes, lean_sarray_cptr(input), input_size, lean_sarray_cptr(output), capacity, &size);
    if (result != kCCSuccess) {
        lean_dec(output);
        return crypto_error(encrypt ? "Could not encrypt the Terraria character." :
            "Could not decrypt the Terraria character.");
    }
    lean_sarray_set_size(output, size);
    return lean_io_result_mk_ok(output);
}
