#!/usr/bin/env bash
# The idea is to sync read flags to the server, then move our deleted
# and archived mails to the appropriate folders using afew, then
# remove the deleted tags (so that they are not resurfaced on threads
# that receive new responses), then sync to the server again, and
# finally incorporate the results of that final sync.
mbsync gmail-inbox gmail-trash gmail-sent seas-inbox seas-trash seas-sent
notmuch new
afew -m
notmuch tag -deleted folder:gmail/trash or folder:seas/trash
mbsync gmail-inbox gmail-trash gmail-sent gmail-archive seas-inbox seas-trash seas-sent seas-archive
notmuch new
afew -t --new
