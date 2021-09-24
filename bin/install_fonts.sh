#!/bin/sh
# Userland mode (~$USER/), (~/).

# ~/.fonts is now deprecated and
# ~/.local/share/fonts should be used instead
FONT_HOME=~/.local/share/fonts

echo "installing fonts to $FONT_HOME"
mkdir -p "$FONT_HOME/adobe-fonts/source-code-pro"

git clone \
   --branch release \
   --depth 1 \
   'https://github.com/adobe-fonts/source-code-pro.git' \
   "$FONT_HOME/adobe-fonts/source-code-pro"

git clone https://gitlab.com/protesilaos/hack-font-mod.git $FONT_HOME/hack

sudo fc-cache -f -v "$FONT_HOME"
