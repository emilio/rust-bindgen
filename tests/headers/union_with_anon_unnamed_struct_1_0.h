// bindgen-flags: --rust-target 1.0 --with-derive-hash --with-derive-partialeq --with-derive-eq

union pixel {
    unsigned int rgba;
    struct {
        unsigned char r;
        unsigned char g;
        unsigned char b;
        unsigned char a;
    };
};
