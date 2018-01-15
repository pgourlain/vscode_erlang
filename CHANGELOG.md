
## Versions
* 0.1.5 (PR #23)
	- variable view tree support for list, map, tuple
	- watch expression support
	- erlang rewrite json serialization and cleanup
	- bug fix: with remove last breakpoint
	- bug fix: runtime set breakpoint in different module
	- bug fix: on break event vscode show file in _build/default/lib/*/src/* not in apps/*/src
* 0.1.4
	- Fix args json escape format, add variable type basic support (#22)
* 0.1.3
	- user can provide configuration settings to the build command (#19)
* 0.1.2
	- fix rebar3 #4
* 0.1.1
	- fix debugger integration #15
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