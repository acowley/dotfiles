#!/usr/bin/env bash
(cd ~/.mail/account.gmail && gmi sync && cd ~/.mail/account.serve && gmi sync) && afew -t -n
