import random
def gen_data(_min, _max, n):
    print([random.randint(_min,_max) for _ in range(n)])

gen_data(0,100,100000)
