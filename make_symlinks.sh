# Symlink the files in this directory to their usual locations in the
# home directory.
DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
ln -s ${DIR}/emacs ${HOME}/.emacs
if [ -d "${HOME}/.gnupg" ]; then
  ln -s ${DIR}/gpg-agent.conf ${HOME}/.gnupg/gpg-agent.conf
else
  echo "${HOME}/.gnupg doesn't exist. Have you setup GPG yet?"
fi
ln -s ${DIR}/org.gnupg.gpg-agent.plist ${HOME}/Library/LaunchAgents/org.gnupg.gpg-agent.plist
