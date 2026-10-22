// Start of values.js
//
// Reading and writing values in the binary data format.  Values are read from
// and written to byte buffers (Uint8Arrays).

var futharkPrimtypes =
  new Set([
    'i8',
    'i16',
    'i32',
    'i64',
    'u8',
    'u16',
    'u32',
    'u64',
    'f16',
    'f32',
    'f64',
    'bool']);


var typToType = { '  i8' : Int8Array ,
                  ' i16' : Int16Array ,
                  ' i32' : Int32Array ,
                  ' i64' : BigInt64Array ,
                  '  u8' : Uint8Array ,
                  ' u16' : Uint16Array ,
                  ' u32' : Uint32Array ,
                  ' u64' : BigUint64Array ,
                  ' f16' : Uint16Array ,
                  ' f32' : Float32Array ,
                  ' f64' : Float64Array ,
                  'bool' : Uint8Array
                };

var typToSize = {
  "bool" : 1,
  "  u8" : 1,
  "  i8" : 1,
  " u16" : 2,
  " i16" : 2,
  " u32" : 4,
  " i32" : 4,
  " f16" : 2,
  " f32" : 4,
  " u64" : 8,
  " i64" : 8,
  " f64" : 8,
}

function toU8(ta) {
  return new Uint8Array(ta.buffer, ta.byteOffset, ta.byteLength);
}

function concat_bytes(arrs) {
  var out = new Uint8Array(arrs.reduce((n, a) => n + a.length, 0));
  var off = 0;
  for (var a of arrs) {
    out.set(a, off);
    off += a.length;
  }
  return out;
}

// The binary encoding of a value with the given primitive type and shape (a
// list of BigInts), where 'bytes' is the payload.
function binary_value(typ, shape, bytes) {
  var ftype = "    ".slice(typ.length) + typ;
  var header = new Uint8Array(7);
  header[0] = 'b'.charCodeAt(0);
  header[1] = 2;
  header[2] = shape.length;
  for (var i = 0; i < 4; i++) {
    header[3+i] = ftype.charCodeAt(i);
  }
  var dims = new BigInt64Array(shape);
  return concat_bytes([header, toU8(dims), bytes]);
}

function is_space(b) {
  return b == 32 || (b >= 9 && b <= 13);
}

class Reader {
  constructor(buff) {
    this.buff = buff;
  }

  // Remove the first n bytes from the buffer, returning a copy of them.
  take(n) {
    if (this.buff.length < n) {
      throw "Unexpected end of input";
    }
    var bytes = this.buff.slice(0, n);
    this.buff = this.buff.subarray(n);
    return bytes;
  }

  read_bin_array(num_dim, typ) {
    var shape = new BigInt64Array(this.take(num_dim * 8).buffer);
    var length = Number(shape.reduce((x, y) => x * y, 1n));
    var data = new (typToType[typ])(this.take(length * typToSize[typ]).buffer);
    return [shape, data];
  }

  read_bin_scalar(typ) {
    return new (typToType[typ])(this.take(typToSize[typ]).buffer)[0];
  }

  skip_spaces() {
    while (this.buff.length > 0 && is_space(this.buff[0])) {
      this.buff = this.buff.subarray(1);
    }
  }

  read_binary(typename, dim) {
    this.skip_spaces();
    var header = this.take(7);
    if (header[0] != 'b'.charCodeAt(0)) {
      throw "Not in binary format"
    }
    var version = header[1];
    if (version != 2) {
      throw "Not version 2";
    }
    var num_dim = header[2];
    var typ = String.fromCharCode(...header.subarray(3, 7));
    var exp_typ = "[]".repeat(dim) + typename;
    var given_typ = "[]".repeat(num_dim) + typ.trim();
    if (exp_typ !== given_typ) {
      throw ("Expected type : " + exp_typ + ", Actual type : " + given_typ);
    }
    if (num_dim === 0) {
      return this.read_bin_scalar(typ);
    } else {
      return this.read_bin_array(num_dim, typ);
    }
  }

  get_buff() {
    return this.buff;
  }
}

// Function is redudant but is helpful for keeping consistent with python implementation
function skip_spaces(reader) {
  reader.skip_spaces();
}

function read_value(typename, reader) {
  var typ = typename;
  var dim = 0;
  while (typ.slice(0, 2) === "[]") {
    dim = dim + 1;
    typ = typ.slice(2);
  }
  if (!futharkPrimtypes.has(typ)) {
    throw ("Unkown type: " + typ);
  }

  var val = reader.read_binary(typ, dim);
  return val;
}

// End of values.js
