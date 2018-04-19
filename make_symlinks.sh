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
