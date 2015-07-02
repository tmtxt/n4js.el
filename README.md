# Neo4j shell for emacs

not finish yet, come back after 1 week

# 1. Dependencies

- `cypher-mode`: can be installed from melpa

# 2. Installation

## Melpa

Coming soon

## Manual install

- Clone it into your `.emacs.d` and add it into load path

```lisp
(add-to-list 'load-path "~/.emacs.d/n4js.el/")
```

- Load the library with `require`

```lisp
(require 'n4js)
```

# 3. Variables

Change these variables based on your demand

- `n4js-cli-program`: the name of neo4j shell cli program, default to
`neo4j-shell`. Change it to absolute path if it's not in your load path.

```lisp
(setq n4js-cli-program "/path/to/neo4j-shell")
```

- `n4js-cli-arguments`: the list of arguments to pass to `neo4j-shell`.

- `n4js-pop-to-buffer`: whether to pop up the neo4j shell buffer after sending
command to execute.

- `n4js-pop-to-buffer-function`: the function used for pop up the neo4j shell
buffer if the above variable is set to t. Default is `pop-to-buffer`. An example
is `pop-to-buffer-same-window` to pop up the neo4j buffer to current window
instead of other window.

- `n4js-font-lock-keywords`: font lock keywords list, default to
`cypher-font-lock-keywords` so that why we need **cypher-mode** as a dependency.
Usually you don't need to change this variable.

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

## Connect to a Neo4j shell with option

If the neo4j shell instance is running with a different port other than the
default 1337, add this to your .emacs

```lisp
(setq n4js-cli-arguments '("-port" "7475"))
```

## Connect to a remote Neo4j shell instance

Usually, passing the argument `-port` and `-host` is not enough if you want to
connect to a neo4j shell that is not running on your computer because neo4j
shell requires some other port for running over RMI. In that case, you can use
the ssh tunnel for connecting

```console
ssh user@host -p 2222 /path/to/neo4j-shell -port 12345
```

To achieve that, add this to your .emacs

```lisp
(setq n4js-cli-program "ssh")
(setq n4js-cli-arguments '("user@host" "-p" "2222" "/path/to/neo4j-shell -port 12345"))
```

## Connect to a Neo4j shell instance inside Vagrant

To connect to a neo4j shell instance running inside Vagrant, use the `vagrant
ssh` command like this

```console
vagrant ssh -c '/path/to/neo4j-shell -port 12345'
```

To achieve that, add this to your .emacs

```lisp
(setq n4js-cli-program "vagrant")
(setq n4js-cli-arguments '("ssh" "-c" "/path/to/neo4j-shell -port 12345"))
```

**Note**: in this case, you need to change to a dired buffer or a file inside
that project folder (or any buffer with `default-directory` inside the project
folder) so that when running the start command, it will run with the cwd is the
`default-directory`. You only need to do this once when you call the command
`n4js-start`.
