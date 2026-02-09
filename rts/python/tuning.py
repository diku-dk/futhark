# Start of tuning.py


def read_tuning_file(kvs, f):
    for line in f.read().splitlines():
        size, value = line.split("=")
        kvs[size] = np.int64(value)


def set_user_params(all_sizes, user_sizes):
    for k, v in user_sizes.items():
        if k in all_sizes:
            all_sizes[k]["value"] = v
        else:
            raise Exception(
                "Unknown tuning parameter: {}\nKnown tuning parameters: {}".format(
                    k, " ".join(all_sizes.keys())
                )
            )


# End of tuning.py.
