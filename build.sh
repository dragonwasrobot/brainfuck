#!/usr/bin/env bash

set -ef -o pipefail

echo "Building..."

# Install node dependencies
npm install

# Compile CSS
npm run css

# Copy FontAwesome
mkdir -p docs/fontawesome
cp -R node_modules/@fortawesome/fontawesome-free/css docs/fontawesome
cp -R node_modules/@fortawesome/fontawesome-free/webfonts docs/fontawesome

# Set flags in index.html
sed -i 's/isProd = false/isProd = true/g' docs/index.html

# Build Elm
js="docs/elm.js"
elm make --optimize --output=$js src/Main.elm

# Minimize compiled JS
min="docs/elm.min.js"
./node_modules/.bin/uglifyjs $js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | ./node_modules/.bin/uglifyjs --mangle --output $min

echo "Finished building."
echo "Build size:    $(cat $js | wc -c) bytes  ($js)"
echo "Minified size: $(cat $min | wc -c) bytes  ($min)"

rm $js
