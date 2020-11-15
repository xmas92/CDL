# C++ Data Language

Initial attempt at creating a Clojure like data language runtime created in C++.

After creating a some working persistent data structures with structural sharing and a compile time parsers which can accept parses but probably was a bit to ambitious to work as intended I restarted the project in Rust: [RDL](https://github.com/xmas92/rdl).

I did learn a bit about C++ though, template language and preprocessor, also implementing a HAMT is always fun.

I've only built this on MSVC with a modified [STL](https://github.com/microsoft/STL/pull/1339).

Everything here is distributed under the MIT License. See `LICENSE` for more information.