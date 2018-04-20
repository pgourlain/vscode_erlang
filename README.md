# Erlang for Visual Studio Code
This extension adds support for the Erlang language to VS Code, including:

## Syntax highlighting
![syntax](images/vscode-erlang-syntax.png)

## Build
![build](images/vscode-erlang-build.png)
- Standard rebar3 is the default built tool, also rebar is supported

## Build arguments
![build](images/vscode-erlang-build-args.png)

- by default "compile" is used in build command
- You can override the default in configuration file (i.e. workspace settings) 

## Available commands
![commands](images/vscode-erlang-commands.png)

## Debugger
![debug](images/vscode-erlang-debug.png)
- Variables list shows variables from the current scope
- Call Stack shows Erlang processes and allows to control them with e.g. Pause and Continue
- Standard commands Step Over, Step Into, Step Out supported
- Full breakpoints support:
  * Regular breakpoints
  * Function Breakpoints: use format module:function/arity
  * Conditional Breakpoints
  * Hit-Count Breakpoints

## Debug arguments  
![debug1](images/vscode-erlang-debug-args.png)
- you can provide a specific command line to 'erl' in launch.json configuration file.

## Run build before debugging
- Add to launch.json file the entry "preLaunchTask": "rebar3 compile"
- Then first time you start debugging you need to:
   1. Select **Configure Task** in the alert, choose **Create tasks.json file from template** and then **Others: Example to run an arbitrary command**
   1. This will create tasks.json for you. Change both label and command to "rebar3 compile".
   1. Add entry "problemMatcher": "$erlang"

Then, before debugging is started, modified files will be recompiled automatically.

## Credits
File 'Erlang.tmLanguage' is inspired from https://github.com/textmate/erlang.tmbundle/blob/master/Syntaxes/Erlang.plist
