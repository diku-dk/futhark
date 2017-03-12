def panic(exitcode, fmt, *args):
    # FIXME: Insert the name of the Futhark program, but I don't know where it can be found
    sys.stderr.write('<Futhark program>: ')
    sys.stderr.write(fmt % args)
    sys.exit(exitcode)
