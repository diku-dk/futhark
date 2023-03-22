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
        for t in self._get_entry_point(entry)[1]:
            print(t)

    def _cmd_outputs(self, args):
        entry = self._get_arg(args, 0)
        for t in self._get_entry_point(entry)[2]:
            print(t)

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
        entry_fname = entry[0]
        num_ins = len(entry[1])
        num_outs = len(entry[2])
        exp_len = 1 + num_outs + num_ins

        if len(args) != exp_len:
            raise self.Failure("Invalid argument count, expected %d" % exp_len)

        out_vnames = args[1 : num_outs + 1]

        for out_vname in out_vnames:
            self._check_new_var(out_vname)

        in_vnames = args[1 + num_outs :]
        ins = [self._get_var(in_vname) for in_vname in in_vnames]

        try:
            (runtime, vals) = getattr(self._ctx, entry_fname)(*ins)
        except Exception as e:
            raise self.Failure(str(e))

        print("runtime: %d" % runtime)

        if num_outs == 1:
            self._vars[out_vnames[0]] = vals
        else:
            for out_vname, val in zip(out_vnames, vals):
                self._vars[out_vname] = val

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
        if typename in self._ctx.opaques:
            vs = []
            for t in self._ctx.opaques[typename]:
                vs += [read_value(t, reader)]
            return opaque(typename, *vs)
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

    def _cmd_entry_points(self, args):
        for k in self._ctx.entry_points.keys():
            print(k)

    _commands = {
        "inputs": _cmd_inputs,
        "outputs": _cmd_outputs,
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
            args = words[1:]
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
