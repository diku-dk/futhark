// Start of wrapperclasses.js

class FutharkArray {
  // 'heap' is a function that returns the typed view of the WASM heap for the
  // element type.  The view must be fetched anew on every use, as it is
  // invalidated when the WASM memory grows.
  constructor(ctx, ptr, type_name, dim, heap, fshape, fvalues, ffree) {
    this.ctx = ctx;
    this.ptr = ptr;
    this.type_name = type_name;
    this.dim = dim;
    this.heap = heap;
    this.fshape = fshape;
    this.fvalues = fvalues;
    this.ffree = ffree;
    this.valid = true;
  }

  validCheck() {
    if (!this.valid) {
      throw "Using freed memory"
    }
  }

  free() {
    this.validCheck();
    this.ffree(this.ctx.ctx, this.ptr);
    this.valid = false;
  }

  // Like values(), this returns a copy, as views of the WASM heap may change.
  shape() {
    this.validCheck();
    var s = this.fshape(this.ctx.ctx, this.ptr) >> 3;
    return this.ctx.wasm.HEAP64.slice(s, s + this.dim);
  }

  async values() {
    this.validCheck();
    var length = Number(this.shape().reduce((a, b) => a * b, 1n));
    var p = this.fvalues(this.ctx.ctx, this.ptr);
    var heap = this.heap();
    var v = p / heap.BYTES_PER_ELEMENT;
    return heap.slice(v, v + length);
  }
}

class FutharkOpaque {
  constructor(ctx, ptr, ffree) {
    this.ctx = ctx;
    this.ptr = ptr;
    this.ffree = ffree;
    this.valid = true;
  }

  validCheck() {
    if (!this.valid) {
      throw "Using freed memory"
    }
  }

  free() {
    this.validCheck();
    this.ffree(this.ctx.ctx, this.ptr);
    this.valid = false;
  }
}

// End of wrapperclasses.js
