Coding Style for Futhark C Runtime Component
==

* Use two spaces for indentation, no tabs.

* Try to stay below 80 characters per line.

* Use snake_case for naming, except preprocessor macros, which are
  uppercase.

* Check all return values.

* Do not use header guards, and do not include one RTS header from
  another.  The header files here are not intended to be used as
  normal C header files, but are instead copied into the generated
  program in a specific order.  An argument could be made that perhaps
  they ought be `.c` files instead.
