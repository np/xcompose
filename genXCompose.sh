#!/bin/bash
cd $(dirname "$0")

# link() @ https://gist.github.com/3181899 {{{1
# Example:
# cd ~/configs
# link .zshrc ~/.zshrc
# link .vimrc ~/.vimrc
link(){
  local dst="$1"
  local ldst="$1"
  local src="$2"
  case "$dst" in
    /*) : ;;
    *) ldst="$(realpath "$dst" --relative-to="$(dirname "$2")")";;
  esac
  if [ -L "$src" ]; then
    # Check if the link is already as expected.
    [ $(readlink "$src") != "$ldst" ] || return 0
    rm "$src"
  elif [ -e "$src" ]; then
    if [ -e "$dst" ]; then
      error 1 "$src already exists, fix this and relaunch"
    else
      echo "moving $src to $dst" >>/dev/stderr
      mv "$src" "$dst"
    fi
  elif [ ! -e "$dst" ]; then
    # if nothing exists we do nothing
    return 0
  fi
  echo "linking $dst" >>/dev/stderr
  ln -s "$ldst" "$src"
}
# }}}

if [ -e /usr/share/X11/locale/en_US.UTF-8/Compose -a -x /usr/bin/ghc ]; then
  echo 'Rebuilding XCompose (the generator)...'
  ghc --make -O2 genXCompose.hs
  echo Running genXCompose...
  ./genXCompose "$@"
  link .XCompose ~/.XCompose
else
  echo Skipping XCompose...
fi

# vim: foldmethod=marker
