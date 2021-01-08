# Start of server.py

import sys
import time

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
            raise self.Failure('Insufficient command args')

    def _get_entry_point(self, entry):
        if entry in self._ctx.entry_points:
            return self._ctx.entry_points[entry]
        else:
            raise self.Failure('Unknown entry point: %s' % entry)

    def _check_var(self, vname):
        if not vname in self._vars:
            raise self.Failure('Unknown variable: %s' % vname)

    def _get_var(self, vname):
        self._check_var(vname)
        return self._vars[vname]

    def _cmd_inputs(self, args):
        entry = self._get_arg(args, 0)
        for t in self._get_entry_point(entry)[0]:
            print(t)

    def _cmd_outputs(self, args):
        entry = self._get_arg(args, 0)
        for t in self._get_entry_point(entry)[1]:
            print(t)

    def _cmd_dummy(self, args):
        pass

    def _cmd_free(self, args):
        for vname in args:
            self._check_var(vname)
            del self._vars[vname]

    def _cmd_call(self, args):
        entry = self._get_entry_point(self._get_arg(args, 0))
        num_ins = len(entry[0])
        num_outs = len(entry[1])

        if len(args) != 1 + num_outs + num_ins:
            raise self.Failure('Invalid argument count, expected %d')

        out_vnames = args[1:num_outs+1]

        for out_vname in out_vnames:
            if out_vname in self._vars:
                raise self.Failure('Variable already exists: %s' % out_vname)

        in_vnames = args[1+num_outs:]
        ins = [ self._get_var(in_vname) for in_vname in in_vnames ]

        try:
            (runtime, vals) = getattr(self._ctx, args[0])(*ins)
        except Exception as e:
            raise self.Failure(str(e))

        print('runtime: %d' % runtime)

        if num_outs == 1:
            self._vars[out_vnames[0]] = vals
        else:
            for (out_vname, val) in zip(out_vnames, vals):
                self._vars[out_vname] = val

    def _cmd_store(self, args):
        fname = self._get_arg(args, 0)

        with open(fname, 'wb') as f:
            for i in range(1, len(args)):
                vname = args[i]
                self._check_var(vname)
                f.write(construct_binary_value(self._vars[vname]))

    def _cmd_restore(self, args):
        if len(args) % 2 == 0:
            raise self.Failure('Invalid argument count')

        fname = args[0]
        args = args[1:]

        with open(fname, 'rb') as f:
            reader = ReaderInput(f)
            while args != []:
                vname = args[0]
                typename = args[1]
                args = args[2:]

                if vname in self._vars:
                    raise self.Failure('Variable already exists: %s' % vname)

                try:
                    self._vars[vname] = read_value(typename, reader)
                except ValueError:
                    raise self.Failure('Failed to restore variable %s.\n'
                                       'Possibly malformed data in %s.\n'
                                       % (vname, fname))

            skip_spaces(reader)
            if reader.get_char() != b'':
                raise self.Failure('Expected EOF after reading values')

    _commands = { 'inputs': _cmd_inputs,
                  'outputs': _cmd_outputs,
                  'call': _cmd_call,
                  'restore': _cmd_restore,
                  'store': _cmd_store,
                  'free': _cmd_free,
                  'clear': _cmd_dummy,
                  'pause_profiling': _cmd_dummy,
                  'unpause_profiling': _cmd_dummy,
                  'report': _cmd_dummy
                 }

    def _process_line(self, line):
        words = line.split()
        if words == []:
            raise self.Failure('Empty line')
        else:
            cmd = words[0]
            args = words[1:]
            if cmd in self._commands:
                self._commands[cmd](self, args)
            else:
                raise self.Failure('Unknown command: %s' % cmd)

    def run(self):
        while True:
            line = sys.stdin.readline()
            if line == '':
                return
            try:
                self._process_line(line)
            except self.Failure as e:
                print('%%% FAILURE')
                print(e.msg)
            print('%%% OK', flush=True)


# End of server.py
