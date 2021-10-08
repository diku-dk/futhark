// Start of wrapperclasses.js

class FutharkArray {
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

  futharkType() {
    return this.type_name;
  }

  free() {
    this.validCheck();
    this.ffree(this.ctx.ctx, this.ptr);
    this.valid = false;
  }

  shape() {
    this.validCheck();
    var s = this.fshape(this.ctx.ctx, this.ptr) >> 3;
    return Array.from(this.ctx.wasm.HEAP64.subarray(s, s + this.dim));
  }

  toTypedArray(dims = this.shape()) {
    this.validCheck();
    console.assert(dims.length === this.dim, "dim=%s,dims=%s", this.dim, dims.toString());
    var length = Number(dims.reduce((a, b) => a * b));
    var v = this.fvalues(this.ctx.ctx, this.ptr) / this.heap.BYTES_PER_ELEMENT;
    return this.heap.subarray(v, v + length);
  }

  toArray() {
    this.validCheck();
    var dims = this.shape();
    var ta = this.toTypedArray(dims);
    return (function nest(offs, ds) {
      var d0 = Number(ds[0]);
      if (ds.length === 1) {
        return Array.from(ta.subarray(offs, offs + d0));
      } else {
        var d1 = Number(ds[1]);
        return Array.from(Array(d0), (x,i) => nest(offs + i * d1, ds.slice(1)));
      }
    })(0, dims);
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
