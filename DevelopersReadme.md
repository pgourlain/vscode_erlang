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


## Run unit tests

In _"Terminal"_ menu choose _"New Terminal"_.
then

```bash
./rebar3 ct
```

## build package

- vsce package : 'vscode:prepublish' is executed


## Deployment

Publish new version to **both** marketplaces (VS Code Marketplace + Open VSX), else Open VSX users stay stuck on old version.

Prereqs (one-time):

```bash
npm install -g vsce ovsx
```

Steps:

1. Bump `version` in [package.json](package.json), commit, tag.
2. Build package:
   ```bash
   ./rebar3 compile
   vsce package
   ```
3. Publish to VS Code Marketplace (needs Azure DevOps PAT):
   ```bash
   vsce publish
   ```
4. Publish to Open VSX (needs token from open-vsx.org, linked via GitHub):
   ```bash
   ovsx publish -p <open-vsx-token>
   ```
   or, from already-built vsix:
   ```bash
   ovsx publish erlang-<version>.vsix -p <open-vsx-token>
   ```

## Language syntax file

See [syntaxes/README.md](syntaxes/README.md).

## References

1. Visual Studio Code [Extension API](https://code.visualstudio.com/api)
