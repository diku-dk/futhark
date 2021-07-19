// Start of array.js

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
  }
  futharkType() { return this.type_name; }
  free() { this.ffree(this.ctx, this.ptr); }
  shape() {
    var s = this.fshape(this.ctx, this.ptr) >> 3;
    return Array.from(HEAP64.subarray(s, s + this.dim));
  }
  toTypedArray(dims = this.shape()) {
    console.assert(dims.length === this.dim, "dim=%s,dims=%s", this.dim, dims.toString());
    var length = Number(dims.reduce((a, b) => a * b));
    var v = this.fvalues(this.ctx, this.ptr) / this.heap.BYTES_PER_ELEMENT;
    return this.heap.subarray(v, v + length);
  }
  toArray() {
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
Module['FutharkArray'] = FutharkArray;

// End of array.js
