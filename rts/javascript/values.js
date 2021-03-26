// Start of values.js
//
//


var type_strs = { "Int8Array" : '  i8',
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
             };
var typToType = { '  i8' : Int8Array ,
              ' i16' : Int16Array ,
              ' i32' : Int32Array ,
              ' i64' : BigInt64Array ,
              '  u8' : BigUint8Array ,
              ' u16' :  BigUint16Array ,
              ' u32' :  BigUint32Array ,
              ' u64' :  BigUint64Array ,
              ' f32' : Float32Array ,
              ' f64' : Float64Array ,
              // TODO implement bool here
             };

function binToStringToArray(str, array) {
  for (var i = 0; i < array.length; i++) {
    array[i] = str.charCodeAt(i);
  }
  return array;
}



function read_binary(str) {
  var str = str.trim();
  if (str.charCodeAt(0) != 'b') {
    throw "Not in binary format"
  }
  str = str.substr(1);
  str = str.trim();
  var version = str.charCodeAt(0);
  if (version != '2') {
    throw "Not version 2";
  }
  str = str.trim();
  var num_dim = parseInt(str.charCodeAt(0));
  str=str.substring(1);
  str = str.trim();
  var typ = str.substr(0, 4);
  str = str.substr(4);
  str.trim();
  if (num_dim == 0) {
    return read_bin_scalar(str, typ);
  } else {
    return read_bin_array(str, num_dim, typ);
  }
}

typToSize = {
  "bool" = 1;
  "  u8" = 1;
  "  i8" = 1;
  " u16" = 2;
  " i16" = 2;
  " u32" = 4;
  " i32" = 4;
  " f32" = 4;
  " u64" = 8;
  " i64" = 8;
  " f64" = 8;
}


function read_bin_scalar(str, typ) {
  var size = typToSize[typ];
  var u8_array = new Uint8Array(size);
  binToStringArray(str, array);
  array = new (typToType.get(typ))(u8_array.buffer);
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

