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
              '  u8' : Uint8Array ,
              ' u16' :  Uint16Array ,
              ' u32' :  Uint32Array ,
              ' u64' :  BigUint64Array ,
              ' f32' : Float32Array ,
              ' f64' : Float64Array ,
              'bool' : Uint8Array
              // TODO implement bool here
             };

function binToStringArray(buff, array) {
  for (var i = 0; i < array.length; i++) {
    array[i] = buff[i];
  }
}

function fileToBuff(fname) {
  var readline = require('readline');
  var fs = require('fs');
  // TODO set decoding flag so there is no wack byte reading
  var buff =  fs.readFileSync(fname);
  return buff;
}

function fileToValue(fname) {
  var str = fileToBuff(fname);
  return read_binary(str);
}

function read_binary(buff) {
  // Skip leading white space
  while (buff.slice(0, 1).toString().trim() == "") {
    buff = this.buff.slice(1);
  }
  if (buff[0] != 'b'.charCodeAt(0)) {
    throw "Not in binary format"
  }
  var version = buff[1];
  if (version != 2) {
    throw "Not version 2";
  }
  var num_dim = buff[2];
  var typ = buff.slice(3, 7);
  buff = buff.slice(7);
  if (num_dim == 0) {
    return read_bin_scalar(buff, typ);
  } else {
    return read_bin_array(buff, num_dim, typ);
  }
}

var typToSize = {
  "bool" : 1,
  "  u8" : 1,
  "  i8" : 1,
  " u16" : 2,
  " i16" : 2,
  " u32" : 4,
  " i32" : 4,
  " f32" : 4,
  " u64" : 8,
  " i64" : 8,
  " f64" : 8,
}

//function read_bin_array(buff, num_dim, typ) {
//  var u8_array = new Uint8Array(num_dim * 8);
//  binToStringArray(buff.slice(0, num_dim * 8), u8_array);
//  shape = new BigInt64Array(u8_array.buffer);
//  var length = shape[0];
//  for (var i = 1; i < shape.length; i++) {
//    length = length * shape[i];
//  }
//  length = Number(length);
//  var dbytes = typToSize[typ];
//  var u8_data = new Uint8Array(length * dbytes);
//  binToStringArray(buff.slice(num_dim * 8, num_dim * 8 + dbytes * length), u8_data);
//  var data  = new (typToType[typ])(u8_data.buffer);
//  // TODO figure out what to return
//  // Pair with (shape, data)
//  // A class?
//  return data;
//}
//
//function read_bin_scalar(buff, typ) {
//  var size = typToSize[typ];
//  var u8_array = new Uint8Array(size);
//  binToStringArray(buff, u8_array);
//  var array = new (typToType[typ])(u8_array.buffer);
//  return array[0];
//}

  


function construct_binary_value(v) {
  var byte_len = v.bytes_per_element();
  var shape = v.shape();
  var values = v.values();
  var elems = 1;
  if (shape != 0) {
    for (var i = 0; i < shape.length; i++) {
      elems = elems * Number(shape[i]);
    }
  }
  var num_bytes = 1 + 1 + 1 + 4 + (shape.length) * 8 + elems * byte_len;

  var bytes = new Uint8Array(num_bytes);
  bytes[0] = Buffer.from('b').readUInt8();
  bytes[1] = 2; // Not sure why this
  bytes[2] = shape.length

  var ftype = v.str_type();

  for (var i = 0; i < 4; i++) {
    bytes[3+i] = ftype.charCodeAt(i);
  }

  if (shape.length > 0) {
    var sizes = new BigUint64Array(shape.length);
    for (var i = 0; i < shape.length; i++) {
      sizes[i] = BigInt(shape[i]);
    }
    var size_bytes = new Uint8Array(sizes.buffer);
    bytes.set(size_bytes, 7);
  }

  var val_bytes = new Uint8Array(values.buffer, values.byteOffset, byte_len * values.length);

  bytes.set(val_bytes, 7 + (shape.length * 8));
  
  //return a buffer needed by appendFile instead of a uint8array
  return Buffer.from(bytes);
}


class Reader {
  constructor(f) {
    this.f = f;
    // TODO update this buff when value is read
    this.buff = fileToBuff(f);
    this.old_buff = this.buff;
  }

  read_bin_array(num_dim, typ) {
    var u8_array = new Uint8Array(num_dim * 8);
    binToStringArray(this.buff.slice(0, num_dim * 8), u8_array);
    var shape = new BigInt64Array(u8_array.buffer);
    var length = shape[0];
    for (var i = 1; i < shape.length; i++) {
      length = length * shape[i];
    }
    length = Number(length);
    var dbytes = typToSize[typ];
    var u8_data = new Uint8Array(length * dbytes);
    binToStringArray(this.buff.slice(num_dim * 8, num_dim * 8 + dbytes * length), u8_data);
    var data  = new (typToType[typ])(u8_data.buffer);
    var tmp_buff = this.buff.slice(num_dim * 8, num_dim * 8 + dbytes * length);
    this.buff = this.buff.slice(num_dim * 8 + dbytes * length);
    // TODO figure out what to return
    // Pair with (shape, data)
    // A class?
    return [shape, data];
  }

  read_bin_scalar(typ) {
    var size = typToSize[typ];
    var u8_array = new Uint8Array(size);
    binToStringArray(this.buff, u8_array);
    var array = new (typToType[typ])(u8_array.buffer);
    this.buff = this.buff.slice(u8_array.length); // Update buff to be unread part of the string
    return array[0];
  }

  skip_spaces() {
    while (this.buff.length > 0 && this.buff.slice(0, 1).toString().trim() == "") {
      this.buff = this.buff.slice(1);
    }
  }

  read_binary() {
    // Skip leading white space
    while (this.buff.slice(0, 1).toString().trim() == "") {
      this.buff = this.buff.slice(1);
    }
    if (this.buff[0] != 'b'.charCodeAt(0)) {
      throw "Not in binary format"
    }
    var version = this.buff[1];
    if (version != 2) {
      throw "Not version 2";
    }
    var num_dim = this.buff[2];
    var typ = this.buff.slice(3, 7);
    this.buff = this.buff.slice(7);
    if (num_dim == 0) {
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
  // TODO include typename in implementation
  var val = reader.read_binary();
  return val;
}
