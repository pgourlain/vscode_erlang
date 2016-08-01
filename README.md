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


## Versions
* 0.0.6
	- Fix 'rebar' spawning on windows
* 0.0.5
	- Add eunit run command without rebar (use an erlang shell)

## Credits
File 'Erlang.tmLanguage' is inspired from https://github.com/textmate/erlang.tmbundle/blob/master/Syntaxes/Erlang.plist
