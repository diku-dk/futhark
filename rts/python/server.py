# Start of server.py

import sys
import time
import shlex  # For string splitting


class Server:
    def __init__(self, ctx):
        self._ctx = ctx
        self._vars = {}

    class Failure(BaseException):
        def __init__(self, msg):
            self.msg = msg

    def _get_arg(self, args, i):
        if i < len(args):
            return args[i]
        else:
            raise self.Failure("Insufficient command args")

    def _get_entry_point(self, entry):
        if entry in self._ctx.entry_points:
            return self._ctx.entry_points[entry]
        else:
            raise self.Failure("Unknown entry point: %s" % entry)

    def _check_var(self, vname):
        if not vname in self._vars:
            raise self.Failure("Unknown variable: %s" % vname)

    def _check_new_var(self, vname):
        if vname in self._vars:
            raise self.Failure("Variable already exists: %s" % vname)

    def _get_var(self, vname):
        self._check_var(vname)
        return self._vars[vname]

    def _cmd_inputs(self, args):
        entry = self._get_arg(args, 0)
        for t in self._get_entry_point(entry)["inputs"]:
            print(t)

    def _cmd_output(self, args):
        entry = self._get_arg(args, 0)
        print(self._get_entry_point(entry)["output"])

    def _cmd_dummy(self, args):
        pass

    def _cmd_free(self, args):
        for vname in args:
            self._check_var(vname)
            del self._vars[vname]

    def _cmd_rename(self, args):
        oldname = self._get_arg(args, 0)
        newname = self._get_arg(args, 1)
        self._check_var(oldname)
        self._check_new_var(newname)
        self._vars[newname] = self._vars[oldname]
        del self._vars[oldname]

    def _cmd_call(self, args):
        entry = self._get_entry_point(self._get_arg(args, 0))
        entry_fname = entry["name"]
        num_ins = len(entry["inputs"])
        exp_len = 2 + num_ins

        if len(args) != exp_len:
            raise self.Failure("Invalid argument count, expected %d" % exp_len)

        out_vname = args[1]

        self._check_new_var(out_vname)

        in_vnames = args[2:]
        ins = [self._get_var(in_vname) for in_vname in in_vnames]

        try:
            runtime, vals = getattr(self._ctx, entry_fname)(*ins)
        except Exception as e:
            raise self.Failure(str(e))

        print("runtime: %d" % runtime)

        self._vars[out_vname] = vals

    def _store_val(self, f, value):
        # In case we are using the PyOpenCL backend, we first
        # need to convert OpenCL arrays to ordinary NumPy
        # arrays.  We do this in a nasty way.
        if isinstance(value, opaque):
            for component in value.data:
                self._store_val(f, component)
        elif (
            isinstance(value, np.number)
            or isinstance(value, bool)
            or isinstance(value, np.bool_)
            or isinstance(value, np.ndarray)
        ):
            # Ordinary NumPy value.
            f.write(construct_binary_value(value))
        else:
            # Assuming PyOpenCL array.
            f.write(construct_binary_value(value.get()))

    def _cmd_store(self, args):
        fname = self._get_arg(args, 0)

        with open(fname, "wb") as f:
            for i in range(1, len(args)):
                self._store_val(f, self._get_var(args[i]))

    def _restore_val(self, reader, typename):
        if typename in self._ctx.opaque_types:
            vs = []
            for t in self._ctx.opaque_types[typename]["payload"]:
                vs += [read_value(t, reader)]
            return self._opaque(typename, vs)
        else:
            return read_value(typename, reader)

    def _cmd_restore(self, args):
        if len(args) % 2 == 0:
            raise self.Failure("Invalid argument count")

        fname = args[0]
        args = args[1:]

        with open(fname, "rb") as f:
            reader = ReaderInput(f)
            while args != []:
                vname = args[0]
                typename = args[1]
                args = args[2:]

                if vname in self._vars:
                    raise self.Failure("Variable already exists: %s" % vname)

                try:
                    self._vars[vname] = self._restore_val(reader, typename)
                except ValueError:
                    raise self.Failure(
                        "Failed to restore variable %s.\n"
                        "Possibly malformed data in %s.\n" % (vname, fname)
                    )

            skip_spaces(reader)
            if reader.get_char() != b"":
                raise self.Failure("Expected EOF after reading values")

    def _cmd_types(self, args):
        for k in self._ctx.opaques.keys():
            print(k)

    # Types and values.
    #
    # An opaque value is represented by its payload: a flat list of primitive
    # values and arrays, as described by 'opaque_types'. A transparent value of
    # type t is represented by a single NumPy value (or a PyOpenCL array).

    def _opaque(self, tname, payload):
        return opaque(tname, self._ctx.opaques, *payload)

    def _opaque_type(self, tname):
        if tname in self._ctx.opaque_types:
            return self._ctx.opaque_types[tname]
        else:
            raise self.Failure("Unknown opaque type: %s" % tname)

    # Split a transparent type name into its rank and primitive type.
    def _split_type(self, tname):
        rank = 0
        while tname.startswith("[]"):
            rank += 1
            tname = tname[2:]
        if tname not in FUTHARK_PRIMTYPES:
            raise self.Failure("Unknown type: %s" % ("[]" * rank + tname))
        return rank, tname

    def _kind(self, tname):
        if tname in self._ctx.opaque_types:
            return self._ctx.opaque_types[tname]["kind"]
        elif self._split_type(tname)[0] == 0:
            return "primitive"
        else:
            return "array"

    def _array_type(self, tname):
        if self._kind(tname) != "array":
            raise self.Failure("Not an array type")
        if tname in self._ctx.opaque_types:
            t = self._ctx.opaque_types[tname]
            return t["rank"], t["elemtype"]
        else:
            return self._split_type(tname)

    def _record_fields(self, tname):
        if self._kind(tname) != "record":
            raise self.Failure("Not a record type")
        return self._opaque_type(tname)["fields"]

    def _sum_variants(self, tname):
        if self._kind(tname) != "sum":
            raise self.Failure("Not a sum type")
        return self._opaque_type(tname)["variants"]

    def _type_of(self, v):
        if isinstance(v, opaque):
            return v.desc
        dtype = v.dtype if hasattr(v, "dtype") else np.dtype(type(v))
        return "[]" * np.ndim(v) + numpy_type_to_type_name(dtype)

    def _get_typed_var(self, vname, tname):
        v = self._get_var(vname)
        if self._type_of(v) != tname:
            raise self.Failure(
                "Variable %s has type %s, expected %s"
                % (vname, self._type_of(v), tname)
            )
        return v

    def _payload_size(self, tname):
        if tname in self._ctx.opaque_types:
            return len(self._ctx.opaque_types[tname]["payload"])
        else:
            return 1

    def _payload(self, v):
        if isinstance(v, opaque):
            return list(v.data)
        else:
            return [v]

    def _from_payload(self, tname, payload):
        if tname in self._ctx.opaque_types:
            return self._opaque(tname, payload)
        else:
            return payload[0]

    # Split a payload into the parts corresponding to the given types.
    def _split_payload(self, tnames, payload):
        parts = []
        for t in tnames:
            n = self._payload_size(t)
            parts += [payload[:n]]
            payload = payload[n:]
        return parts

    # An empty value of the given transparent type, used for the unused
    # parts of the payload of a sum type.
    def _blank(self, tname):
        rank, pt = self._split_type(tname)
        dtype = FUTHARK_PRIMTYPES[pt]["numpy_type"]
        if rank == 0:
            return dtype(0)
        else:
            return np.zeros((0,) * rank, dtype=dtype)

    # Convert a PyOpenCL array to a NumPy array.
    def _to_host(self, x):
        if isinstance(x, np.ndarray) or not hasattr(x, "get"):
            return x
        else:
            return x.get()

    def _shape(self, v, rank):
        return tuple(int(d) for d in np.shape(self._payload(v)[0])[:rank])

    def _parse_ints(self, args):
        try:
            return [int(x) for x in args]
        except ValueError:
            raise self.Failure("Invalid integer in: %s" % " ".join(args))

    # Index a transparent array, producing a fresh value.
    def _index_transparent(self, x, idx):
        x = self._to_host(x[idx])
        if isinstance(x, np.ndarray):
            return x[()] if x.ndim == 0 else x.copy()
        else:
            return x

    def _cmd_kind(self, args):
        print(self._kind(self._get_arg(args, 0)))

    def _cmd_type(self, args):
        print(self._type_of(self._get_var(self._get_arg(args, 0))))

    def _cmd_rank(self, args):
        print(self._array_type(self._get_arg(args, 0))[0])

    def _cmd_elemtype(self, args):
        print(self._array_type(self._get_arg(args, 0))[1])

    def _cmd_shape(self, args):
        v = self._get_var(self._get_arg(args, 0))
        rank, _ = self._array_type(self._type_of(v))
        for d in self._shape(v, rank):
            print(d)

    def _cmd_new_array(self, args):
        dst = self._get_arg(args, 0)
        tname = self._get_arg(args, 1)
        self._check_new_var(dst)
        rank, et = self._array_type(tname)
        dims = tuple(self._parse_ints(args[2 : 2 + rank]))
        if len(dims) != rank or any(d < 0 for d in dims):
            raise self.Failure("Expected %d valid dimensions" % rank)
        vnames = args[2 + rank :]
        if len(vnames) != np.prod(dims, dtype=np.int64):
            raise self.Failure(
                "Expected %d values, but got %d"
                % (np.prod(dims, dtype=np.int64), len(vnames))
            )
        elems = [self._payload(self._get_typed_var(v, et)) for v in vnames]
        if tname in self._ctx.opaque_types:
            ts = self._ctx.opaque_types[tname]["payload"]
        else:
            ts = [tname]
        payload = []
        for i, t in enumerate(ts):
            r, pt = self._split_type(t)
            dtype = FUTHARK_PRIMTYPES[pt]["numpy_type"]
            if elems == []:
                payload += [np.zeros(dims + (0,) * (r - rank), dtype=dtype)]
            else:
                try:
                    x = np.array(
                        [self._to_host(e[i]) for e in elems], dtype=dtype
                    )
                except ValueError:
                    raise self.Failure("Array elements have irregular shapes")
                payload += [x.reshape(dims + x.shape[1:])]
        self._vars[dst] = self._from_payload(tname, payload)

    def _cmd_set(self, args):
        arr = self._get_arg(args, 0)
        v = self._get_var(arr)
        rank, et = self._array_type(self._type_of(v))
        val = self._payload(self._get_typed_var(self._get_arg(args, 1), et))
        idx = tuple(self._parse_ints(args[2:]))
        if len(idx) != rank:
            raise self.Failure(
                "%d indices expected but %d values provided."
                % (rank, len(idx))
            )
        shape = self._shape(v, rank)
        if not all(0 <= i < d for i, d in zip(idx, shape)):
            raise self.Failure(
                "Index %s out of bounds for shape %s" % (idx, shape)
            )
        payload = [self._to_host(x) for x in self._payload(v)]
        for x, y in zip(payload, val):
            x[idx] = self._to_host(y)
        self._vars[arr] = self._from_payload(self._type_of(v), payload)

    def _cmd_index(self, args):
        dst = self._get_arg(args, 0)
        v = self._get_var(self._get_arg(args, 1))
        self._check_new_var(dst)
        rank, et = self._array_type(self._type_of(v))
        idx = tuple(self._parse_ints(args[2:]))
        if len(idx) != rank:
            raise self.Failure(
                "%d indices expected but %d values provided."
                % (rank, len(idx))
            )
        shape = self._shape(v, rank)
        if not all(0 <= i < d for i, d in zip(idx, shape)):
            raise self.Failure(
                "Index %s out of bounds for shape %s" % (idx, shape)
            )
        payload = [self._index_transparent(x, idx) for x in self._payload(v)]
        self._vars[dst] = self._from_payload(et, payload)

    def _cmd_zip(self, args):
        dst = self._get_arg(args, 0)
        tname = self._get_arg(args, 1)
        self._check_new_var(dst)
        rank, _ = self._array_type(tname)
        fields = self._opaque_type(tname).get("fields")
        if fields is None:
            raise self.Failure("Cannot zip to this array type")
        vnames = args[2:]
        if len(vnames) != len(fields):
            raise self.Failure(
                "%d arrays expected but %d values provided."
                % (len(fields), len(vnames))
            )
        vs = [self._get_typed_var(v, t) for v, (_, t) in zip(vnames, fields)]
        if len(set(self._shape(v, rank) for v in vs)) > 1:
            raise self.Failure("Arrays have different shapes")
        payload = []
        for v in vs:
            payload += self._payload(v)
        self._vars[dst] = self._opaque(tname, payload)

    def _cmd_unzip(self, args):
        v = self._get_var(self._get_arg(args, 0))
        tname = self._type_of(v)
        self._array_type(tname)
        fields = self._opaque_type(tname).get("fields")
        if fields is None:
            raise self.Failure("Cannot unzip this array type")
        dsts = args[1:]
        if len(dsts) != len(fields):
            raise self.Failure(
                "%d arrays expected but %d values provided."
                % (len(fields), len(dsts))
            )
        for dst in dsts:
            self._check_new_var(dst)
        ts = [t for _, t in fields]
        for dst, t, p in zip(
            dsts, ts, self._split_payload(ts, self._payload(v))
        ):
            self._vars[dst] = self._from_payload(t, p)

    def _cmd_fields(self, args):
        for f, t in self._record_fields(self._get_arg(args, 0)):
            print(f, t)

    def _cmd_new(self, args):
        dst = self._get_arg(args, 0)
        tname = self._get_arg(args, 1)
        self._check_new_var(dst)
        fields = self._record_fields(tname)
        vnames = args[2:]
        if len(vnames) != len(fields):
            raise self.Failure(
                "%d fields expected but %d values provided."
                % (len(fields), len(vnames))
            )
        payload = []
        for v, (_, t) in zip(vnames, fields):
            payload += self._payload(self._get_typed_var(v, t))
        self._vars[dst] = self._opaque(tname, payload)

    def _cmd_project(self, args):
        dst = self._get_arg(args, 0)
        v = self._get_var(self._get_arg(args, 1))
        field = self._get_arg(args, 2)
        self._check_new_var(dst)
        fields = self._record_fields(self._type_of(v))
        ts = [t for _, t in fields]
        for (f, t), p in zip(
            fields, self._split_payload(ts, self._payload(v))
        ):
            if f == field:
                self._vars[dst] = self._from_payload(t, p)
                return
        raise self.Failure("No such field: %s" % field)

    def _cmd_variants(self, args):
        for name, payload in self._sum_variants(self._get_arg(args, 0)):
            print(name)
            for t, _ in payload:
                print("- %s" % t)

    # The variant of a sum-typed value. When there is more than one variant,
    # the first element of the payload is the index of the variant.
    def _variant_of(self, v):
        variants = self._sum_variants(self._type_of(v))
        if len(variants) == 1:
            return variants[0]
        else:
            return variants[int(v.data[0])]

    def _cmd_construct(self, args):
        dst = self._get_arg(args, 0)
        tname = self._get_arg(args, 1)
        vname = self._get_arg(args, 2)
        self._check_new_var(dst)
        variants = self._sum_variants(tname)
        for i, (name, vpayload) in enumerate(variants):
            if name == vname:
                break
        else:
            raise self.Failure("No such variant: %s" % vname)
        vnames = args[3:]
        if len(vnames) != len(vpayload):
            raise self.Failure(
                "%d values expected but %d provided."
                % (len(vpayload), len(vnames))
            )
        ts = self._opaque_type(tname)["payload"]
        payload = [self._blank(t) for t in ts]
        if len(variants) > 1:
            payload[0] = FUTHARK_PRIMTYPES[ts[0]]["numpy_type"](i)
        for v, (t, js) in zip(vnames, vpayload):
            for j, x in zip(js, self._payload(self._get_typed_var(v, t))):
                payload[j] = x
        self._vars[dst] = self._opaque(tname, payload)

    def _cmd_destruct(self, args):
        v = self._get_var(self._get_arg(args, 0))
        _, vpayload = self._variant_of(v)
        dsts = args[1:]
        if len(dsts) != len(vpayload):
            raise self.Failure(
                "%d variables expected but %d provided."
                % (len(vpayload), len(dsts))
            )
        for dst in dsts:
            self._check_new_var(dst)
        for dst, (t, js) in zip(dsts, vpayload):
            self._vars[dst] = self._from_payload(t, [v.data[j] for j in js])

    def _cmd_variant(self, args):
        print(self._variant_of(self._get_var(self._get_arg(args, 0)))[0])

    def _cmd_attributes(self, args):
        return self._get_entry_point(self._get_arg(args, 0))["attributes"]

    def _cmd_entry_points(self, args):
        for k in self._ctx.entry_points.keys():
            print(k)

    _commands = {
        "inputs": _cmd_inputs,
        "output": _cmd_output,
        "call": _cmd_call,
        "restore": _cmd_restore,
        "store": _cmd_store,
        "free": _cmd_free,
        "rename": _cmd_rename,
        "clear": _cmd_dummy,
        "pause_profiling": _cmd_dummy,
        "unpause_profiling": _cmd_dummy,
        "report": _cmd_dummy,
        "types": _cmd_types,
        "entry_points": _cmd_entry_points,
        "kind": _cmd_kind,
        "type": _cmd_type,
        "rank": _cmd_rank,
        "elemtype": _cmd_elemtype,
        "shape": _cmd_shape,
        "new_array": _cmd_new_array,
        "set": _cmd_set,
        "index": _cmd_index,
        "zip": _cmd_zip,
        "unzip": _cmd_unzip,
        "fields": _cmd_fields,
        "new": _cmd_new,
        "project": _cmd_project,
        "variants": _cmd_variants,
        "construct": _cmd_construct,
        "destruct": _cmd_destruct,
        "variant": _cmd_variant,
        "attributes": _cmd_attributes,
    }

    def _process_line(self, line):
        lex = shlex.shlex(line)
        lex.quotes = '"'
        lex.whitespace_split = True
        lex.commenters = ""
        words = list(lex)
        if words == []:
            raise self.Failure("Empty line")
        else:
            cmd = words[0]
            args = [w.strip('"') for w in words[1:]]
            if cmd in self._commands:
                self._commands[cmd](self, args)
            else:
                raise self.Failure("Unknown command: %s" % cmd)

    def run(self):
        while True:
            print("%%% OK", flush=True)
            line = sys.stdin.readline()
            if line == "":
                return
            try:
                self._process_line(line)
            except self.Failure as e:
                print("%%% FAILURE")
                print(e.msg)


# End of server.py
