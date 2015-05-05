# Codex

[![Build Status](https://travis-ci.org/CommonDoc/codex.svg?branch=master)](https://travis-ci.org/CommonDoc/codex)

A documentation system for Common Lisp.

# Overview

**Codex** generates documentation. You just write docstrings as usual, only
using [VerTeX][vertex] syntax, append a couple of extra files (A few tutorials
and what have you), and Codex creates beautiful online documentation, and in the
near future, PDF manuals.

Codex uses [CommonDoc][commondoc] for representing the documentation and
converting it to other formats.

# Usage

## Macros

### `cl:with-package`

Sets the current package for node insertion. See below.

### `cl:doc`

Insert the documentation of a symbol. For example, if your app defines a class
`my-class` in the package `pack`, the following invocation will expand to class
documentation, including the docstring of the class itself, and documentation of
its slots:

```
(Scriba syntax)
@cl:with-package[name=pack](
  @cl:doc(class my-class)
)

(VerTeX syntax)
\cl:with-package[pack=pack]{
  \cl:doc{class my-class}
}
```

### `cl:param`

Refers to an argument of a function, macro or method. For example

```
(Scriba syntax)
The @cl:param(list) argument holds...

(VerTeX syntax)
The \cl:param{points} argument is a vector of points of...
```

# Themes

# Implementation

Codex uses [docparser][docparser] to extract documentation from systems, and
[Pandocl][pandocl] to parse docstrings and files into a CommonDoc document.

[vertex]: https://github.com/CommonDoc/vertex
[commondoc]: https://github.com/CommonDoc/common-doc
[clhs]: http://www.lispworks.com/documentation/HyperSpec/Front/
[docparser]: https://github.com/eudoxia0/docparser
[pandocl]: https://github.com/CommonDoc/pandocl

# License

Copyright (c) 2014-2015 Fernando Borretti

Licensed under the MIT License.
