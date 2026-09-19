# Install Erlang/OTP

The extension runs its language server and debugger on your Erlang/OTP
installation, so `erl` and `escript` must be reachable.

- **macOS**: `brew install erlang`
- **Debian/Ubuntu**: `sudo apt install erlang`
- **Windows**: installer from [erlang.org/downloads](https://www.erlang.org/downloads)
- **Several OTP versions side by side**: [kerl](https://github.com/kerl/kerl) or [asdf](https://asdf-vm.com/)

When `erl` is not on the `PATH` (kerl, asdf or a custom build), set
`erlang.erlangPath` to the directory that holds it, for example
`/home/me/kerl/26.2/bin`.

The status bar shows the language server state and the OTP version it
runs on once it has started.
