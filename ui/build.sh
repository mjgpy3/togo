#!/usr/bin/env bash

spago bundle-app --main Main --to index.js && parcel build index.html --public-url /ui
