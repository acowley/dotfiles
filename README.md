dotfiles
========

emacs
---

This includes the emacs configuration I use on a few OS X machines. It
is focused on Haskell development tools, and personalized to my
tastes.

The file is structured using
[outshine](https://github.com/tj64/outshine) for quick navigation and
editing as an `org` buffer.

All packages should be automatically installed from MELPA if they are
not already installed.

gpg-agent
---

Some sketchy work to getting gpg-agent to work nicely on OS X so that
one can work with encrypted files without repeatedly entering a
password.

After `launchctl` starts the `gpg-agent` daemon, you will need to,
`export $(cat ~/.gpg-agent-info)` to get the shell environment
configured correctly.

You will also need to make sure the `use-agent` line is uncommented in
`~/.gnupg/gpg.conf`.

The agent is configured to use a custom pinentry program that invokes
`emacsclient` for password entry. Compile the program, `ghc Main.hs -o
hpinentry`, in the `pinentry` directory for this to work.

The reason for the custom pinentry program is that, normally, when you
try to use `gpg` at the terminal, a full-window PIN entry UI opens
up. This doesn't work from within emacs, so you need to do it at the
terminal, then do whatever you were going to do in emacs, and you
won't need to supply your password thanks to the agent.
