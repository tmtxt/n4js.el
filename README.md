# Neo4j shell for emacs

# 1. Dependencies

- (optional) `cypher-mode`: can be installed from melpa. If installed
  it's font lock definitions are used.

# 2. Installation

## [Melpa](https://melpa.org/#/n4js)

## Manual install

- Clone the repository (or download the ZIP file and unzip it), and
  add its directory to the load path (if it's not already there)

```lisp
(add-to-list 'load-path "~/.emacs.d/n4js.el/")
```

- Load the library with `require`

```lisp
(require 'n4js)
```

- or use `use-package` or similar library:

```lisp
(use-package n4js
   :custom (n4js-user "neo4j")
           (n4js-address "bolt://example.com:7867"))
```

# 3. User-customizable variables

See `M-x customize-group n4js`.

# 4. Commands

- `n4js-start`: start a neo4j shell process
- `n4js-send-current-region`: send the active region to neo4j shell process
- `n4js-send-buffer`: send the whole buffer to neo4j shell process
- `n4js-send-paragraph`: send the paragraph at point to neo4j shell process
- `n4js-send-region-or-buffer`: send the current region if active, otherwise send
the whole buffer to neo4j shell process
- `n4js-send-dwim`: send the current region if active, otherwise send the
paragraph at point to neo4j shell process
- `n4js-switch-to-buffer`: switch to neo4j shell buffer if exist, otherwise,
start a new one

# 5. Tips

## Connect to a Neo4j shell with non-default settings

`n4js-address` allows the user to specify adress, port, and protocol
to use to access the cypher shell. `n4js-username` and `n4js-password`
allow the user to easily set these too. Other settings should use the
`n4js-cli-arguments` variable.
