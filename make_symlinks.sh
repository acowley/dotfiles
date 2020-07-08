# Symlink the files in this directory to their usual locations in the
# home directory.
DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
ln -s ${DIR}/emacs ${HOME}/.emacs
if [ ! -d "${HOME}/.emacs.d" ]; then
  mkdir "${HOME}/.emacs.d"
fi
ln -s ${DIR}/early-init.el ${HOME}/.emacs.d/early-init.el
if [ -d "${HOME}/.gnupg" ]; then
  ln -s ${DIR}/gpg-agent.conf ${HOME}/.gnupg/gpg-agent.conf
else
  echo "${HOME}/.gnupg doesn't exist. Have you setup GPG yet?"
fi
ln -s ${DIR}/org.gnupg.gpg-agent.plist ${HOME}/Library/LaunchAgents/org.gnupg.gpg-agent.plist
ln -s ${DIR}/.mbsyncrc ${HOME}/.mbsyncrc
if [ ! -d "${HOME}/.stack" ]; then
  mkdir "$HOME/.stack"
fi
ln -s "${DIR}/stack-config.yaml" "${HOME}/.stack/config.yaml"
if [ ! -d "${HOME}/.config/nixpkgs" ]; then
  mkdir -p "${HOME}/.config/nixpkgs"
fi
ln -s ${DIR}/config.nix "${HOME}/.config/nixpkgs/config.nix"
ln -s ${DIR}/overlays.nix "${HOME}/.config/nixpkgs/overlays.nix"
ln -s ${DIR}/yasnippets/ "${HOME}/.emacs.d/snippets"
ln -s ${DIR}/dictrc "${HOME}/.dictrc"

ln -s ${DIR}/notmuch-config "${HOME}/.notmuch-config"
if [ ! -d "${HOME}/.config/afew" ]; then
  mkdir -p "${HOME}/.config/afew"
fi
ln -s ${DIR}/afew-config "${HOME}/.config/afew/config"

