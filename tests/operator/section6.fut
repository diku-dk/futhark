-- Test that parameter names are not lost in sections.

def flipreplicate x n = replicate n x

def main n (x: i32) = (x `flipreplicate`) n
