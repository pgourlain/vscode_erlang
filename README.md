# Erlang for Visual Studio Code
This extension adds support for the Erlang language to VS Code, including:

* Colorization
![syntax](images/vscode-erlang-syntax.png)

* Build
![build](images/vscode-erlang-build.png)
	- only rebar is supported
	- I provide a sample of tasks.json for rebar, look at 'samples' directory

* Available commands
![commands](images/vscode-erlang-commands.png)

* Debug
![debug](images/vscode-erlang-debug.png)
	- Variables list is automatically refresh from the current scope
	- The process list contains an additional process for communication with vscode

* Debug arguments  
![debug1](images/vscode-erlang-debug-args.png)
	- you can provide a specific command line to 'erl' in launch.json configuration file.



## Versions
* 0.1.1
	- fix debugger integration
* 0.1.0
	- debugger integration
* 0.0.9
	- fix shortcuts (ctrl-shift-B, ctrl-shift-T)
* 0.0.8
	- Add debugger adapter  
* 0.0.7
	- Fix assertXXXX expected/value for vscode.diagnostic 
* 0.0.6
	- Fix 'rebar' spawning on windows
* 0.0.5
	- Add eunit run command without rebar (use an erlang shell)

## Credits
File 'Erlang.tmLanguage' is inspired from https://github.com/textmate/erlang.tmbundle/blob/master/Syntaxes/Erlang.plist
