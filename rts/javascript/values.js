// Start of values.js
//
//

class ReaderInput {
  constructor(f) {
    this.f = f;
    this.lookahead_buffer = [];
  }

  get_char() {
    if (this.lookaheaad_buffer.length == 0) {
      // TODO find the js equivalent of read()
      return this.f.read(1);
    } else {
      c = this.lookahead_buffer[0];
      this.lookahead_buffer = this.lookahead_buffer.slice(1);
      return c;
    }
  }

  unget_char(c) {
    this.lookahead_buffer = [c].concat(this.lookahead_buffer);
  }

  get_chars(n) {
    var n1 = Math.min(n, this.lookahead_buffer.length);
    // TODO make sure this is done properly with binary value
    var s = this.lookahead_buffer.slice(0, n1).join("");
    this.lookahead_buffer = this.lookahead_buffer.slice(n1);
    var n2 = n - n1;
    if (n2 > 0) {
      //TODO another instance of python read
      s = s + this.f.read(n2);
    }
    return s;
  }


  get peek_char() {
    var c = this.get_char();
    if (c) {
      unget_char(c);
    }
    return c;
  }
}


function skip_spaces(f) {
  var c = f.get_char();
  // TODO get JS version of None
  while (c != None) {
    // TODO get JS equivalent of isspace
    if (c.isspace()) {
      c = f.get_char();
    } 
    else if (c


    


