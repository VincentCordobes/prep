#!/bin/bash

get_os () {
  if [[ "$OSTYPE" == "linux-gnu" ]]; then
    echo "linux"
  elif [[ "$OSTYPE" == "darwin"* ]]; then
    echo "macos"
  elif [[ "$OSTYPE" == "cygwin" ]]; then
    echo "windows"
  elif [[ "$OSTYPE" == "msys" ]]; then
    echo "windows"
  elif [[ "$OSTYPE" == "win32" ]]; then
    echo "windows"
  elif [[ "$OSTYPE" == "freebsd"* ]]; then
    echo "linux"
  else
    echo "Unknown os"
    exit -1
  fi
}

cd /tmp
os=`get_os`

if [[ "$os" == "windows" ]]; then
  echo "Sorry, Windows is not yet supported :("
  exit -1
fi

echo "Downloading latest ${os} release..."
curl -sLo prep.tar.gz "https://github.com/VincentCordobes/prep/releases/latest/download/prep-${os}.tar.gz"

echo "Installing binaries..."
tar -xzf prep.tar.gz
rm prep.tar.gz
chmod u+x prep*
sudo mv prep* /usr/local/bin/

echo "Installing zsh completion"
curl -sLo /usr/local/share/zsh/site-functions/_prep \
  "https://raw.githubusercontent.com/VincentCordobes/prep/master/scripts/_prep"

if [ -d /etc/bash_completion.d/ ]; then
  echo "Installing bash completion..."
  curl -sLo /etc/bash_completion.d/prep.sh \
    "https://raw.githubusercontent.com/VincentCordobes/prep/master/scripts/complete.sh"
  source /etc/bash_completion.d/prep.sh
fi

if [ -d /usr/local/share/man/man1/ ]; then
  echo "Installing man pages..."
  prep --help=groff > /usr/local/share/man/man1/prep.1
fi

echo "prep $(prep --version) installed!"
