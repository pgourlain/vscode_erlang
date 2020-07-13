
# Introduction
This file contains some help about Erlang extension. 


# Changing Debugger Mode
- Debugger Mode setting is read at extension startup, so when you change this setting, you must restart vscode.

# How exclude directories from "goto definition" feature

Add in your local settings.json this section

```
{
    ...
    "search.exclude" : {
        "**/_build": true
    }
    ...
}
```