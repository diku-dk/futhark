#!/usr/bin/env python3

import subprocess
import re
import os
import json

ARTIFACT_FILE = "unittests/Futhark/Analysis/Properties/Artifact.hs"

def run_command(cmd, suppress_output=True):
    """
    Runs a shell command and returns its stdout, stderr, and exit code.
    Optionally suppresses output to the console.
    """
    try:
        process = subprocess.run(
            cmd,
            shell=True,
            capture_output=True,
            text=True,
            check=False
        )
        stdout = process.stdout
        stderr = process.stderr
        exit_code = process.returncode
        if not suppress_output and (stdout or stderr):
            print(stdout, end='')
            print(stderr, end='')
        return stdout, stderr, exit_code
    except Exception as e:
        print(f"Error running command '{cmd}': {e}")
        return "", str(e), 1

def extract_time_from_output(output):
    """
    Extracts the 'real' time from the output of the 'time -p' command.
    """
    match = re.search(r"real\s+(\d+\.?\d*)", output)
    if match:
        return float(match.group(1))
    return None

def main():
    benchmark_results = {}
    print("Benchmarking Futhark 'verify' command for IndexFn properties...")
    print("------------------------------------------------------------------")

    # Extract program paths
    awk_cmd_part = "awk -F'\"' '{print $2}'"
    stdout, _, exit_code = run_command(f"grep -o '\".*\\.fut\"' \"{ARTIFACT_FILE}\" | {awk_cmd_part}", suppress_output=True)
    if exit_code != 0:
        print(f"Error extracting program paths from {ARTIFACT_FILE}. Exiting.")
        exit(1)

    program_paths = [path.strip() for path in stdout.split('\n') if path.strip()]

    if not program_paths:
        print(f"No program paths found in {ARTIFACT_FILE}. Exiting.")
        exit(1)

    for program_path in program_paths:
        print(f"Running verify for: {program_path}")

        # Run futhark verify with timing
        # Using time -p for portable time output (real, user, sys)
        # We redirect stdout to /dev/null as in the shell script
        verify_cmd = f"{{ time -p futhark verify '{program_path}' > /dev/null; }} 2>&1"
        verify_time_output, _, _ = run_command(verify_cmd, suppress_output=True)
        verify_time = extract_time_from_output(verify_time_output)

        if verify_time is None:
            print(f"  Failed to get verification timing for {program_path}. Output: {verify_time_output}")
            continue

        unannotated_program_path = program_path.replace(".fut", "_unannotated.fut")
        print(f"Running futhark cuda for: {unannotated_program_path}")

        # Run futhark cuda with timing
        compile_cmd = f"{{ time -p futhark cuda '{unannotated_program_path}' > /dev/null; }} 2>&1"
        compile_time_output, _, compile_exit_status = run_command(compile_cmd, suppress_output=True)
        compile_time = extract_time_from_output(compile_time_output)

        if compile_exit_status != 0:
            print(f"  futhark cuda failed for {unannotated_program_path} with exit status {compile_exit_status}. Aborting processing for this program.")
            continue
        if compile_time is None:
            print(f"  Failed to get compile timing for {unannotated_program_path}. Output: {compile_time_output}")
            continue

        print(f"  Verification time taken: {verify_time:.1f}s")
        print(f"  Compile (without verification) time taken: {compile_time:.2f}s")

        total_time = verify_time + compile_time
        print(f"  Total compile time taken (with verification): {total_time:.2f}s")

        if total_time == 0:
            print("  Total time is zero, cannot calculate percentage.")
        else:
            percentage = (verify_time / total_time) * 100
            print(f"  Verification time is {percentage:.0f}% of total (compile + verification) time.")

        program_name_with_ext = os.path.basename(program_path)
        program_name = program_name_with_ext.removesuffix(".fut")
        benchmark_results[program_name] = (f"{verify_time:.1f}s", f"{percentage:.0f}%")

        print("------------------------------------------------------------------")

    print("Benchmarking complete.")

    output_filename = "verification_times.json"
    with open(output_filename, 'w') as f:
        json.dump(benchmark_results, f, indent=4)
    print(f"\nBenchmark results written to {output_filename}")

if __name__ == "__main__":
    main()
