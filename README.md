# Codex

A documentation system for Common Lisp.

# Overview

**Codex** generates documentation. You just write docstrings as usual, only
using [VerTeX][vertex] syntax, append a couple of extra files (A few tutorials
and what have you), and Codex creates beautiful online documentation, and in the
near future, PDF manuals.

Codex uses [CommonDoc][commondoc] for representing the documentation and
converting it to other formats.

# Usage

## Tags

### `clref`

**VerTeX Syntax:** `\clref{<package>:<symbol>}`

Reference a symbol by name. The name must have the full package name.

The package name determines what kind of link will be generated:

* If the symbol is part of the Common Lisp package, a link to the
  [Common Lisp HyperSpec][clhs] will be generated.
* If the symbol comes from a package that's part of the project, a link to the
  symbol in the documentation will be generated.

**Examples:**

```tex
... we use the \clref{cl:find} function in \clref{myapp:my-package} to find...
```

### `cldoc`

**VerTeX Syntax:** `\cldoc{<package>:<symbol>}`

Insert the documentation of a symbol. For example, if your app defines a class
`my-class` in the package `pack`, invoking the `cldoc` macro with
`pack:my-class` will expand to class documentation, including the docstring of
the class itself, and documentation of its slots.

### `param`

**TeX Syntax:** `\param{name}`

Refers to an argument of a function, macro or method.

**Examples:**

```tex
... the \param{points} argument is a vector of points of...
```

# Themes

# Implementation

Codex uses [Quickdocs's][qd] [parser][qd-parser] system to extract
documentation from systems.

Each package is a separate document.

[vertex]: https://github.com/CommonDoc/vertex
[commondoc]: https://github.com/CommonDoc/common-doc
[clhs]: http://www.lispworks.com/documentation/HyperSpec/Front/
[qd]: http://quickdocs.org/
[qd-parser]: https://github.com/fukamachi/quickdocs/blob/master/quickdocs-parser.asd

# License

Copyright (c) 2014 Fernando Borretti

Licensed under the MIT License.
