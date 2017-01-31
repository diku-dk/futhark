#include <inttypes.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

//#include "reader.h"
//#include "panic.h"

// taken from http://esr.ibiblio.org/?p=5095
#define IS_BIG_ENDIAN (*(uint16_t *)"\0\xff" < 0x100)

#define READ_BINARY_VERSION 1

// As defined in src/Futhark/Representation/Primitive.hs
typedef enum {
    FUTHARK_INT8 = 0,
    FUTHARK_INT16 = 1,
    FUTHARK_INT32 = 2,
    FUTHARK_INT64 = 3,
    FUTHARK_FLOAT32 = 4,
    FUTHARK_FLOAT64 = 5,
    FUTHARK_BOOL = 6
} futhark_primtype;

const char *FUTHARK_PRIMTYPE_NAMES[] = {
    "i8",
    "i16",
    "i32",
    "i64",
    "f32",
    "f64",
    "bool"
};

const int FUTHARK_PRIMTYPE_SIZES[] = {
    1,
    2,
    4,
    8,
    4,
    8,
    1
};

////////////////////////////////////////////////////////////////////////////////
// Little endian
////////////////////////////////////////////////////////////////////////////////

static int read_byte(void* dest) {
    int num_bytes_read = read(STDIN_FILENO, dest, 1);
    return num_bytes_read == 1 ? 0 : 1;
}

static int read_le_2byte(void* dest) {
    int num_bytes_read = read(STDIN_FILENO, dest, 2);
    return num_bytes_read == 2 ? 0 : 1;
}

static int read_le_4byte(void* dest) {
    int num_bytes_read = read(STDIN_FILENO, dest, 4);
    return num_bytes_read == 4 ? 0 : 1;
}

static int read_le_8byte(void* dest) {
    int num_bytes_read = read(STDIN_FILENO, dest, 8);
    return num_bytes_read == 8 ? 0 : 1;
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
    int c = getchar();
    if (c == 'b') {
        int bin_version;
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


static int read_int8(void* dest) {
    if (read_is_binary()) {
        return read_byte(dest);
    }
    return read_str_int8(dest);
}

static int read_int16(void* dest) {
    if (read_is_binary()) {
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
        if (IS_BIG_ENDIAN) {
            return read_be_8byte(dest);
        } else {
            return read_le_8byte(dest);
        }
    }
    return read_str_int64(dest);
}

static int read_char(void* dest) {
    if (read_is_binary()) {
        return read_byte(dest);
    }
    return read_str_char(dest);
}

static int read_double(void* dest) {
    if (read_is_binary()) {
        if (IS_BIG_ENDIAN) {
            return read_be_8byte(dest);
        } else {
            return read_le_8byte(dest);
        }
    }
    return read_str_double(dest);
}

static int read_float(void* dest) {
    if (read_is_binary()) {
        if (IS_BIG_ENDIAN) {
            return read_be_4byte(dest);
        } else {
            return read_le_4byte(dest);
        }
    }
    return read_str_float(dest);
}

static int read_bool(void* dest) {
    if (read_is_binary()) {
        return read_byte(dest);
    }
    return read_str_bool(dest);
}

////////////////////////////////////////////////////////////////////////////////
// General array interface
////////////////////////////////////////////////////////////////////////////////

static int read_array(int64_t elem_enum, int64_t elem_size, int (*elem_reader)(void*),
                      const char *type_name, void **data, int64_t *shape, int64_t dims) {
    if (!read_is_binary()) {
        return read_str_array(elem_size, elem_reader, type_name, data, shape, dims);
    }

    // now we know it is binary :)
    int ret;

    int bin_elem_enum;
    ret = read_byte(&bin_elem_enum);

    if (ret != 0) { panic(1, "binary-input: Couldn't get elem_enum.\n"); }

    if (bin_elem_enum != elem_enum) {
        panic(1, "binary-input: Expected array with type %s but got array with type %s.\n",
              type_name, FUTHARK_PRIMTYPE_NAMES[bin_elem_enum]);
    }

    int expected_size = FUTHARK_PRIMTYPE_SIZES[elem_enum];
    if (expected_size != elem_size) {
        panic(1, "binary-input: Expected type %s used %i bytes per element, but call to `read_array` tell me that type %s uses %i bytes per element. Enum used is %i\n",
              FUTHARK_PRIMTYPE_NAMES[elem_enum], FUTHARK_PRIMTYPE_SIZES[elem_enum],
              type_name, elem_size, elem_enum);
    }

    int bin_dims;
    ret = read_byte(&bin_dims);
    if (ret != 0) { panic(1, "binary-input: Couldn't get dims.\n"); }

    if (bin_dims != dims) {
        panic(1, "binary-input: Expected %i dimensions, but got array with %i dimensions.\n",
              dims, bin_dims);
    }

    int64_t elem_count = 1;
    for (int i=0; i<dims; i++) {
        int64_t bin_shape;
        ret = IS_BIG_ENDIAN ? read_be_8byte(&bin_shape) : read_le_8byte(&bin_shape);
        if (ret != 0) { panic(1, "binary-input: Couldn't read size for dimension %i of array.\n", i); }
        elem_count *= bin_shape;
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
        for(int64_t i=0; i<elem_count; i++){
            ret = elem_reader(datac);
            if (ret != 0) { panic(1, "binary-input: error reading element %i.\n", i); }
            datac += elem_size;
        }
    } else {
        int64_t num_elems_read = fread(*data, elem_size, elem_count, stdin);
        if (num_elems_read != elem_count) {
            panic(1, "binary-input: tried to read %i elements of an array, but only got %i elements.\n",
                  elem_count, num_elems_read);
        }
    }

    return 0;

    // TODO: Should consider adding a null byte to the end of all binary
    // inputs. as a small way to make sure the binary input was actually
    // supposed to end here

    // Should document file format. And make a small test to see if it works ;)
}
