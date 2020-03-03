# Work with language syntax files

According Visual Studio Code Syntax Highlight Guide:
> As a grammar grows more complex, it can become difficult to understand and
> maintain it as json. If you find yourself writing complex regular expressions
> or needing to add comments to explain aspects of the grammar, consider using
> yaml to define your grammar instead.
>
> Yaml grammars have the exact same structure as a json based grammar but allow
> you to use yaml's more concise syntax, along with features such as multi-line
> strings and comments.
>
> VS Code can only load json _(or plist)_ grammars, so yaml based grammars must
> be converted to json _(or plist)_.

To work easier with TextMate YAML language files install VSCode extension
[TextMate Languages](https://marketplace.visualstudio.com/items?itemName=Togusa09.tmlanguage)
from _Ben Hockley_.

* Use commands _"Convert to YAML-tmLanguage file"_ and
  _"Convert to tmLanguage file"_ provided by extension _TextMate Languages_.

* Alternatively download
  [plist-yaml-plist](https://github.com/grahampugh/plist-yaml-plist)
  from Github and use below commands: (On Windows use WSL)

      /path/to/plist_yaml.py erlang.tmLanguage erlang.tmLanguage.yaml
      # Edit YAML file here ... then if you are ready convert back to PLIST
      /path/to/yaml_plist.py erlang.tmLanguage.yaml erlang.tmLanguage

## References

1. Visual Studio Code [Syntax Highlight Guide](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide)
