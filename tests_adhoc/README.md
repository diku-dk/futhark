# Ad-hoc tests

This directory contains ad-hoc tests for things that are not covered by one of
the more systematic test suites.

To add an ad-hoc test, simply add a subdirectory that contains an executable
shell script `test.sh.` Failure is indicated by a nonzero exit code, and
hopefully some kind of explanation on standard error.
