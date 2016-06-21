# Stub code for OpenCL setup.

import pyopencl as cl

def get_prefered_context(interactive=False, platform_pref=None, device_pref=None):
    if interactive:
        return cl.create_some_context(interactive=True)

    def platform_ok(p):
        return not platform_pref or p.name.find(platform_pref) >= 0
    def device_ok(d):
        return not device_pref or d.name.find(device_pref) >= 0

    for p in cl.get_platforms():
        if not platform_ok(p):
            continue
        for d in p.get_devices():
            if not device_ok(d):
                continue
            return cl.Context(devices=[d])
    raise Exception('No OpenCL platform and device matching constraints found.')
