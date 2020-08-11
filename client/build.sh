#!/bin/bash

node node_modules/typescript/bin/tsc
node node_modules/webpack-cli/bin/cli.js
mkdir public/bundle
mv dist/main.js public/bundle/main.js
