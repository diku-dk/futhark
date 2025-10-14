-- #1920

def n = 3i64

def bar f = f {xs = replicate n 0f32}

def main = bar id
