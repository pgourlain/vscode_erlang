
# Change log

## Version 0.2.6 (May 2, 2018)
### LSP : Help hover for standard functions and some LSP fixes 
* [PR #52](https://github.com/pgourlain/vscode_erlang/pull/52) : Hover sample
![lsp](images/vscode-erlang-hover.png)

Thanks to 
* [Wojtek Surowka](https://github.com/wojteksurowka)
---

## Version 0.2.5 (May 1, 2018)
### LSP Improvements 
* [PR #47](https://github.com/pgourlain/vscode_erlang/pull/49)
	- LSP Improvements (navigation and lint)
	
Thanks to 
* [Wojtek Surowka](https://github.com/wojteksurowka)
---

## Version 0.2.4 (April 29, 2018)
### LSP fixes 
* [PR #47](https://github.com/pgourlain/vscode_erlang/pull/47)
	- fix crash after remove epp error
* [PR #48](https://github.com/pgourlain/vscode_erlang/pull/48)
	- Go to Definition for function calls 
	
	
Thanks to 
* [Wojtek Surowka](https://github.com/wojteksurowka)
* [Shian](https://github.com/shian)
---

## Version 0.2.3 (April 21, 2018)
### LSP fixes 
* [PR #46](https://github.com/pgourlain/vscode_erlang/pull/46)
	- LSP fixes
	- README update
	
Thanks to [Wojtek Surowka](https://github.com/wojteksurowka)

---

## Version 0.2.2 (March 27, 2018)
### Debugging Fix and improvements 
* [PR #42](https://github.com/pgourlain/vscode_erlang/pull/43)
* [PR #43](https://github.com/pgourlain/vscode_erlang/pull/44)
* [PR #45](https://github.com/pgourlain/vscode_erlang/pull/45)
	- Conditional breakpoints
	- Warnings recognised by problemMatcher
	- Minor changes
	
Thanks to [Wojtek Surowka](https://github.com/wojteksurowka)

---


## Version 0.2.1 (March 24, 2018)
### Debugging Fix and improvements 
* [PR #42](https://github.com/pgourlain/vscode_erlang/pull/42)
	- Step Out support
	- Pause support
	- Call stack shows function names with arity
	- Other minor improvements and bug fixes

Thanks to [Wojtek Surowka](https://github.com/wojteksurowka)

---

## Version 0.2.0 (March 17, 2018)
### Debugging Fix and improvements 
- [PR #37](https://github.com/pgourlain/vscode_erlang/pull/37), [PR #39](https://github.com/pgourlain/vscode_erlang/pull/39), [PR #40](https://github.com/pgourlain/vscode_erlang/pull/40)

Thanks to [Wojtek Surowka](https://github.com/wojteksurowka)

## Previous versions
* 0.1.9 Linter fix : (disable epp warnings)
* 0.1.8 First preview of Validation/Format document
	- ![lsp](images/lsp_validate_document.png)
		- Today, you should activate 'Auto save' in the 'File' menu of VSCode. Due that only the filename of document is send to the Erlang LSP server (LSP: Language Server Protocol)
		- technical traces can be activated by configuration (set  "erlang.LanguageServerProtocol.verbose" : "true" in your settings file) 
	- Console quiet by default (https://github.com/wojteksurowka) -> [PR #29](https://github.com/pgourlain/vscode_erlang/pull/29)
	- Start Without Debugging support (https://github.com/wojteksurowka) -> [PR #28](https://github.com/pgourlain/vscode_erlang/pull/28) 
* 0.1.7 Fix missing file
* 0.1.6 Fixes for process management during debugging ([wojteksurowka](https://github.com/wojteksurowka) -> [PR #25,#26,#27](https://github.com/pgourlain/vscode_erlang/pull/27))
	- correct file opened from stack without release) structure
	- Fix for processes monitoring
	- do not output waiting status to console
* 0.1.5 debug experience improvements, bug fixes, clear code ([Andrew Sumskoy](https://github.com/andrewsumskoy) -> [PR #23](https://github.com/pgourlain/vscode_erlang/pull/23))
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