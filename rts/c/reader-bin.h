// taken from http://esr.ibiblio.org/?p=5095
#define IS_BIG_ENDIAN (*(uint16_t *)"\0\xff" < 0x100)

#define READ_BINARY_VERSION 1

typedef struct {
    const char binname[4]; // used for parsing binary date
    const char* type_name; // used for printing
    const int size;
} primtype_info_t;

const primtype_info_t FUTHARK_PRIMTYPES[] = {
    {.binname = "  i8", .type_name = "i8",   .size = 1},
    {.binname = " i16", .type_name = "i16",  .size = 2},
    {.binname = " i32", .type_name = "i32",  .size = 4},
    {.binname = " i64", .type_name = "i64",  .size = 8},
    {.binname = " f32", .type_name = "f32",  .size = 4},
    {.binname = " f64", .type_name = "f64",  .size = 8},
    {.binname = "bool", .type_name = "bool", .size = 1},
};

// These indices should match up with the information above
typedef enum {
    FUTHARK_INT8 = 0,
    FUTHARK_INT16 = 1,
    FUTHARK_INT32 = 2,
    FUTHARK_INT64 = 3,
    FUTHARK_FLOAT32 = 4,
    FUTHARK_FLOAT64 = 5,
    FUTHARK_BOOL = 6,

    // Please add new types above this line -- we exploit that enums are just
    // ints, and use this value to loop through all types we know.
    FUTHARK_NUM_PRIMTYPES
} primtype_t;


////////////////////////////////////////////////////////////////////////////////
// Little endian
////////////////////////////////////////////////////////////////////////////////

static int read_byte(void* dest) {
    int num_elems_read = fread(dest, 1, 1, stdin);
    return num_elems_read == 1 ? 0 : 1;
}

static int read_le_2byte(void* dest) {
    int num_elems_read = fread(dest, 2, 1, stdin);
    return num_elems_read == 1 ? 0 : 1;
}

static int read_le_4byte(void* dest) {
    int num_elems_read = fread(dest, 4, 1, stdin);
    return num_elems_read == 1 ? 0 : 1;
}

static int read_le_8byte(void* dest) {
    int num_elems_read = fread(dest, 8, 1, stdin);
    return num_elems_read == 1 ? 0 : 1;
}

////////////////////////////////////////////////////////////////////////////////
// Big endian
////////////////////////////////////////////////////////////////////////////////

static int read_be_2byte(void* dest) {
    char* destc = (char*) dest;
    int num_matched = scanf("%c%c", destc+1, destc+0);
    return num_matched == 2 ? 0 : 1;
}

static int read_be_4byte(void* dest) {
    char* destc = (char*) dest;
    int num_matched = scanf("%c%c%c%c", destc+3, destc+2, destc+1, destc+0);
    return num_matched == 4 ? 0 : 1;
}

static int read_be_8byte(void* dest) {
    char* destc = (char*) dest;
    int num_matched = scanf("%c%c%c%c%c%c%c%c", destc+7, destc+6, destc+5,
                            destc+4, destc+3, destc+2, destc+1, destc+0);
    return num_matched == 8 ? 0 : 1;
}

////////////////////////////////////////////////////////////////////////////////
// General value interface
////////////////////////////////////////////////////////////////////////////////

static int read_is_binary() {
    skipspaces();
    int c = getchar();
    if (c == 'b') {
        int8_t bin_version;
        int ret = read_byte(&bin_version);

        if (ret != 0) { panic(1, "binary-input: could not read version.\n"); }

        if (bin_version != READ_BINARY_VERSION) {
            panic(1, "binary-input: File uses version %i, but I only understand version %i.\n",
                  bin_version, READ_BINARY_VERSION);
        }

        return 1;
    }
    ungetc(c, stdin);
    return 0;
}

static primtype_t read_bin_read_type_enum() {
    char read_binname[4];

    int num_matched = scanf("%4c", read_binname);
    if (num_matched != 1) { panic(1, "binary-input: Couldn't read element type.\n"); }

    for (int i=0; i<FUTHARK_NUM_PRIMTYPES; i++) {
        // I compare the 4 characters manually instead of using strncmp because
        // this allows any value to be used, also NULL bytes
        if ( (read_binname[0] == FUTHARK_PRIMTYPES[i].binname[0]) &&
             (read_binname[1] == FUTHARK_PRIMTYPES[i].binname[1]) &&
             (read_binname[2] == FUTHARK_PRIMTYPES[i].binname[2]) &&
             (read_binname[3] == FUTHARK_PRIMTYPES[i].binname[3]) ) {
            return i;
        }
    }
    panic(1, "binary-input: Did not recognize the type '%s'.\n", read_binname);
}

static void read_bin_ensure_scalar(primtype_t expected_type) {
    int8_t bin_dims;
    int ret = read_byte(&bin_dims);
    if (ret != 0) { panic(1, "binary-input: Couldn't get dims.\n"); }

    if (bin_dims != 0) {
        panic(1, "binary-input: Expected scalar (0 dimensions), but got array with %i dimensions.\n",
              bin_dims);
    }

    primtype_t bin_type_enum = read_bin_read_type_enum();
    if (bin_type_enum != expected_type) {
        panic(1, "binary-input: Expected scalar of type %s but got scalar of type %s.\n",
              FUTHARK_PRIMTYPES[expected_type].type_name,
              FUTHARK_PRIMTYPES[bin_type_enum].type_name);
    }
}

static int read_int8(void* dest) {
    if (read_is_binary()) {
        read_bin_ensure_scalar(FUTHARK_INT8);
        return read_byte(dest);
    }
    return read_str_int8(dest);
}

static int read_int16(void* dest) {
    if (read_is_binary()) {
        read_bin_ensure_scalar(FUTHARK_INT16);
        if (IS_BIG_ENDIAN) {
            return read_be_2byte(dest);
        } else {
            return read_le_2byte(dest);
        }
    }
    return read_str_int16(dest);
}

static int read_int32(void* dest) {
    if (read_is_binary()) {
        read_bin_ensure_scalar(FUTHARK_INT32);
        if (IS_BIG_ENDIAN) {
            return read_be_4byte(dest);
        } else {
            return read_le_4byte(dest);
        }
    }
    return read_str_int32(dest);
}

static int read_int64(void* dest) {
    if (read_is_binary()) {
                read_bin_ensure_scalar(FUTHARK_INT64);
        if (IS_BIG_ENDIAN) {
            return read_be_8byte(dest);
        } else {
            return read_le_8byte(dest);
        }
    }
    return read_str_int64(dest);
}

static int read_float(void* dest) {
    if (read_is_binary()) {
        read_bin_ensure_scalar(FUTHARK_FLOAT32);
        if (IS_BIG_ENDIAN) {
            return read_be_4byte(dest);
        } else {
            return read_le_4byte(dest);
        }
    }
    return read_str_float(dest);
}

static int read_double(void* dest) {
    read_bin_ensure_scalar(FUTHARK_FLOAT64);
    if (read_is_binary()) {
        if (IS_BIG_ENDIAN) {
            return read_be_8byte(dest);
        } else {
            return read_le_8byte(dest);
        }
    }
    return read_str_double(dest);
}

static int read_bool(void* dest) {
    if (read_is_binary()) {
        read_bin_ensure_scalar(FUTHARK_BOOL);
        return read_byte(dest);
    }
    return read_str_bool(dest);
}

////////////////////////////////////////////////////////////////////////////////
// General array interface
////////////////////////////////////////////////////////////////////////////////

static int read_array(primtype_t expected_type, int64_t elem_size, int (*elem_reader)(void*),
                      const char *type_name, void **data, int64_t *shape, int64_t dims) {
    if (!read_is_binary()) {
        return read_str_array(elem_size, elem_reader, type_name, data, shape, dims);
    }

    // now we know it is binary :)
    int ret;

    int8_t bin_dims;
    ret = read_byte(&bin_dims);
    if (ret != 0) { panic(1, "binary-input: Couldn't get dims.\n"); }

    if (bin_dims != dims) {
        panic(1, "binary-input: Expected %i dimensions, but got array with %i dimensions.\n",
              dims, bin_dims);
    }

    primtype_t bin_type_enum = read_bin_read_type_enum();
    const primtype_info_t bin_primtype = FUTHARK_PRIMTYPES[bin_type_enum];
    if (expected_type != bin_type_enum) {
        panic(1, "binary-input: Expected %iD-array with element type '%s' but got %iD-array with element type '%s'.\n",
              dims, FUTHARK_PRIMTYPES[expected_type].type_name, dims, bin_primtype.type_name);
    }

    if (elem_size != bin_primtype.size) {
        panic(1, "binary-input: The RTS expected type %s to use %i bytes per element, but the call to `read_array` tells me to use %i bytes per element.\n",
              bin_primtype.type_name, bin_primtype.size, elem_size);
    }

    uint64_t elem_count = 1;
    for (int i=0; i<dims; i++) {
        uint64_t bin_shape;
        ret = IS_BIG_ENDIAN ? read_be_8byte(&bin_shape) : read_le_8byte(&bin_shape);
        if (ret != 0) { panic(1, "binary-input: Couldn't read size for dimension %i of array.\n", i); }
        elem_count *= bin_shape;
        shape[i] = (int64_t) bin_shape;
    }

    void* tmp = realloc(*data, elem_count * elem_size);
    if (tmp == NULL) {
        panic(1, "binary-input: Failed to allocate array of size %i.\n",
              elem_count * elem_size);
    }
    *data = tmp;

    if (IS_BIG_ENDIAN && elem_size != 1) {
        switch (elem_size) {
        case 2: { elem_reader = &read_be_2byte; break; }
        case 4: { elem_reader = &read_be_4byte; break; }
        case 8: { elem_reader = &read_be_8byte; break; }
        default:
            panic(1, "binary-input: Currently can't read element of size %i bytes on big endian platform.\n",
                  elem_size);
            break;
        }

        char* datac = (char*) *data;
        for(uint64_t i=0; i<elem_count; i++){
            ret = elem_reader(datac);
            if (ret != 0) { panic(1, "binary-input: error reading element %i.\n", i); }
            datac += elem_size;
        }
    } else {
        size_t num_elems_read = fread(*data, elem_size, elem_count, stdin);
        if (num_elems_read != elem_count) {
            panic(1, "binary-input: tried to read %i elements of an array, but only got %i elements.\n",
                  elem_count, num_elems_read);
        }
    }

    return 0;
}
