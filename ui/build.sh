#!/usr/bin/env bash

spago bundle-app --main Main --to index.js && ./node_modules/.bin/parcel build index.html --public-url /ui
