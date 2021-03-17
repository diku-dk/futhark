// Start of values.js
//
//

type_strs = { "Int8Array" : '  i8',
              "Int16Array" : ' i16',
              "Int32Array" : ' i32',
              "BigInt64Array" : ' i64',
              "BigUint8Array" : '  u8',
              "BigUint16Array" :  ' u16',
              "BigUint32Array" :  ' u32',
              "BigUint64Array" :  ' u64',
              "Float32Array" : ' f32',
              "Float64Array" : ' f64',
              // TODO implement bool here
             }

function construct_binary_value(v) {

  var bytes = v.bytes_per_elem();
  var shape = v.shape();
  var values = v.values();
  var elems = 1;
  for (var i = 0; i < shape.length; i++) {
    elems = elems * Number(shape[i]);
  }
  var num_bytes = 1 + 1 + 1 + 4 + shape.length * 8 + elems * bytes;


  var bytes = new Uint8Array(num_bytes);
  bytes[0] = Buffer.from('b').readUInt8();
  bytes[1] = 2; // Not sure why this
  bytes[2] = shape.length

  var ftype = type_strs[v.str_type()];

  for (var i = 0; i < 4; i++) {
    bytes[3+i] = ftype.charCodeAt(i);
  }

  var sizes = new BigInt64Array(shape);
  var size_bytes = new Uint8Array(sizes.buffer);
  bytes.set(size_bytes, 7);

  var val_bytes = new Uint8Array(values.buffer);
  bytes.set(val_bytes, 7 + (shape.length * 8));
  
  return bytes;
}

