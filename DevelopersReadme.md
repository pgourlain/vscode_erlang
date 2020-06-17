# Developer's Readme

## Build

At the very first time install some packages in project directory:
(make sure [Node.js](https://nodejs.org) is installed on your machine)

    cd /path/to/vscode_erlang
    npm install
    npm install -g vsce

Build the extension and create a VSIX package for manual distributing:

    ./rebar3 compile
    vsce package

## Test

In _"Run"_ sidbar choose _"Launch Extension"_.

## Language syntax file

See [syntaxes/README.md](syntaxes/README.md).

## References

1. Visual Studio Code [Extension API](https://code.visualstudio.com/api)
