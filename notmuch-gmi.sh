#!/usr/bin/env bash
(cd ~/.mail/account.gmail && gmi sync && cd ~/.mail/account.seas && gmi sync) && afew -t -n
