# Emacs tools for OpenCode V2

Ports the seven tools from the OMP Emacs tools and adds a read-only
cursor-context query. The shared Emacs Lisp implementation lives at
`../../elisp/emacs-tools.el`; `../../omp/emacs-tools.el` is a compatibility
symlink for the OMP extension. Both agents share the same Emacs functions,
image buffer, and image list.

## Install globally

From any directory:

```sh
mkdir -p "${XDG_CONFIG_HOME:-$HOME/.config}/opencode/plugins"
ln -s "$HOME/dotfiles/opencode/emacs-tools" \
  "${XDG_CONFIG_HOME:-$HOME/.config}/opencode/plugins/emacs-tools"
```

OpenCode discovers the plugin package automatically. No npm installation is
needed. The dotfiles `make_symlinks.sh` also sets up this link. Configuration
directories are watched; reopen the session if its tool catalog hasn't updated.

`emacsclient` must be on the **OpenCode server's** PATH, and Emacs must already
be running a server (`M-x server-start`, or `(server-start)` in your init file).
The plugin deliberately does not start another editor when the server is absent.
On a remote OpenCode server these tools interact with that server's Emacs, not
your local desktop. In this Nix setup the CLI executable is `opencode2`.

Alternatively, add the absolute path to `index.ts` under `plugins` in your
existing `~/.config/opencode/opencode.json(c)`. To select another client or Lisp
file, use a plugin entry with options:

```jsonc
{
  "$schema": "https://opencode.ai/config.json",
  "plugins": [{
    "package": "/Users/acowley/dotfiles/opencode/emacs-tools/index.ts",
    "options": {
      "emacsclient": "/Users/acowley/.nix-profile/bin/emacsclient",
      "elispPath": "/Users/acowley/dotfiles/elisp/emacs-tools.el"
    }
  }]
}
```

Use either automatic discovery or explicit configuration, not both.

## Use

These are agent tools, **not slash commands**. Ask OpenCode naturally, for example:

- “Show `plot.png` in Emacs.”
- “Compare these three images in Emacs.”
- “Flash lines 20–35 of `src/main.cpp` in Emacs.”
- “List the headings in `notes.org`, then read the Hardware subtree.”
- “Where is the cursor in Emacs?”

| Tool | Arguments |
| --- | --- |
| `emacs_open_image` | `filepath` |
| `emacs_open_images` | `filepaths` (nonempty array) |
| `emacs_flash_code` | `filepath`, `start_line`, `end_line` (inclusive) |
| `emacs_org_list_headings` | `filepath`, optional `max_depth` |
| `emacs_org_get_subtree` | `filepath`, `heading_pattern` |
| `emacs_org_get_heading_at_line` | `filepath`, `line_number` |
| `emacs_org_search_headings` | `filepath`, `pattern` |
| `emacs_cursor_context` | none |

Paths are relative to the invoking session's directory. Patterns use **Emacs
regular expression syntax**. Org queries prefer an existing buffer, including
its unsaved edits, and otherwise read the file without opening a visible buffer.
Image viewing and highlighting preserve the original focus behavior.
`emacs_cursor_context` reports the selected window's buffer, file (or none),
1-based absolute line and 1-based column, plus the containing Org heading when
available. If the selected buffer is an `agent-shell-mode` buffer (including
the buffer named `OpenCode Agent`), it reports the most recently visited
non-agent-shell buffer instead. It reads only existing editor state and does not
move point or change the selected window.

OpenCode checks the corresponding `emacs_*` permission action for each tool.
For example, `{ "action": "emacs_*", "resource": "*", "effect": "ask" }`
requires approval for these tools. These custom tools use tool-level permissions,
not the built-in `read` tool's path checks; they can read any file accessible to
Emacs. No arbitrary Lisp evaluator is exposed to the agent.

The bridge loads its Lisp definitions on first use, loads the plugin-local
cursor helper on cursor-context calls (including when the OMP feature was
already loaded), requires Org explicitly,
quotes strings and list arguments, executes asynchronously with cancellation
and a 10-second timeout, and cleans up its private result file. It fixes the
OMP extension's cold-start ordering without changing that extension.

## Tests

With Bun and Emacs on PATH, run `bun test` in this directory. Integration tests
use isolated batch Emacs processes, not your running editor.
