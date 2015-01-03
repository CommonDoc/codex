# Codex

A documentation system for Common Lisp.

# Overview

**Codex** generates documentation. You just write docstrings as usual, only using
[VerTeX][vertex] syntax, append a couple of extra files (A few tutorials and
what have you), and Codex creates beautiful online documentation as well as
offline manuals.

Codex uses [CommonDoc][commondoc] and [Pandocl][pandocl] for representing the
documentation and converting it to other formats, respectively.

# Usage

## Tags

### `clref`

**TeX Syntax:** `\clref{<package>:<symbol>}`

Reference a symbol by name. The name must have the full package name.

The package name determines what kind of link will be generated:

* If the symbol is part of the Common Lisp package, a link to the
  [Common Lisp HyperSpec][clhs] will be generated.
* If the symbol comes from a package that's part of the project, a link to the
  symbol in the proper document will be generated.
* If the symbol belongs to an external package, no link is generated.

**Examples:**

```tex
... we use the \clref{cl:find} function in \clref{myapp:my-function} to find...
```

### `arg`

**TeX Syntax:** `\arg{name}`

Refers to a parameter of a function, macro or method.

**Examples:**

```tex
... the \arg{points} argument is a vector of points of...
```

# Themes

# Implementation

Codex uses [Quickdocs's][qd] [parser][qd-parser] system to extract
documentation from systems.

Each package is a separate document.

[vertex]: https://github.com/CommonDoc/vertex
[commondoc]: https://github.com/CommonDoc/common-doc
[pandocl]: https://github.com/CommonDoc/pandocl
[clhs]: http://www.lispworks.com/documentation/HyperSpec/Front/
[qd]: http://quickdocs.org/
[qd-parser]: https://github.com/fukamachi/quickdocs/blob/master/quickdocs-parser.asd

# License

Copyright (c) 2014 Fernando Borretti

Licensed under the MIT License.
