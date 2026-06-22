// Start of values.js

const futhark_binary_format_version = 2;

class FutharkReader {
  constructor(buf) {
    futhark_assert(buf instanceof Uint8Array);
    this.buf = buf;
  }

  seek(n) {
    this.buf = this.buf.subarray(n);
  }

  read_byte() {
    const b = this.buf[0];
    this.seek(1);
    return b;
  }

  read_i64() {
    const buf = new Uint8Array(this.buf.subarray(0, 8));
    const val = new BigInt64Array(buf.buffer, 0, 1)[0];
    this.seek(8);
    return val;
  }

  read_value(expected_type = undefined) {
    let off = 0;
    while (this.is_whitespace(this.buf[off])) off++;
    this.seek(off);

    futhark_assert(this.read_byte() == this.byte_val('b'),
      "Expected binary input");
    futhark_assert(this.read_byte() == futhark_binary_format_version,
      "Can only read binary format version " + futhark_binary_format_version);
    
    const rank = this.read_byte();

    const type = String.fromCodePoint(...this.buf.slice(0, 4)).trimStart();
    futhark_assert(type in primInfos, "Unknown type: " + type);
    this.seek(4);

    if (expected_type != undefined) {
      if (rank == 0 && expected_type != type) {
        throw new Error(`Read unexpected type '${rank}d ${type}', expected ${expected_type}`);
      }
      if (rank > 0) {
        let expected_rank = 0;
        let rem_type = expected_type;
        while (rem_type.startsWith("[]")) {
          expected_rank++;
          rem_type = rem_type.slice(2);
        }

        if (rank != expected_rank || type != rem_type) {
          throw new Error(`Read unexpected type '${rank}d ${type}', expected ${expected_type}`);
        }
      }
    }

    let shape = [];
    for (let i = 0; i < rank; i++) {
      shape.push(this.read_i64());
    }

    if (rank == 0) { 
      const [val, _] = this.read_array(type, [1n]);
      return val[0];
    }
    else { 
      return this.read_array(type, shape);
    }
  }

  read_array(type, shape) {
    const type_info = primInfos[type];
    const flat_len = Number(shape.reduce((a, b) => a * b));

    const buf = new Uint8Array(this.buf.subarray(0, flat_len * type_info.size));
    const wrapper = new type_info.array_type(buf.buffer, 0, flat_len);

    this.seek(wrapper.byteLength);
    return [wrapper, shape];
  }

  is_whitespace(b) {
    const whitespace = [' ', '\t', '\n'].map((c) => this.byte_val(c));
    return b in whitespace;
  }

  byte_val(c) { return c.charCodeAt(0); }
}

class FutharkWriter {
  encode_value(val, type) {
    let elem_type = undefined;
    let rank = 0;
    let flat_len = 0;

    if (type in primInfos) {
      elem_type = type;
      rank = 0;
      flat_len = 1;
    }
    else {
      elem_type = type.replaceAll("[]", "");
      const [data, shape] = val;
      rank = shape.length;
      flat_len = Number(shape.reduce((a, b) => a * b));
    }
  
    const prim_info = primInfos[elem_type];
    const header_size = 3 + 4;
    const total_size = header_size + rank * 8 + flat_len * prim_info.size;

    const buf = new Uint8Array(total_size);
    buf[0] = this.byte_val('b');
    buf[1] = futhark_binary_format_version;
    buf[2] = rank;

    const tag = Uint8Array.from(prim_info.tag, c => c.charCodeAt(0));
    buf.set(tag, 3);
    
    let offset = header_size;

    let data = undefined;
    let shape = undefined;
    if (rank == 0) {
      data = new prim_info.array_type([val]);
      shape = [];
    }
    else {
      const [d, s] = val;
      data = d;
      shape = s;
    }

    const dims = new BigInt64Array(shape);
    buf.set(new Uint8Array(dims.buffer), offset);
    offset += dims.byteLength;

    const bin_data = new Uint8Array(data.buffer, data.byteOffset, data.byteLength);
    buf.set(bin_data, offset);

    return buf;
  }

  byte_val(c) { return c.charCodeAt(0); }
}

// End of values.js
