#!/bin/bash

# requires prez
# npm install -g prez

set -e

git init
prez --init .
echo '' > css/custom.css
echo '' > js/custom.js
rm -r images/*
rm -r slides/*

cat << EOF > .gitignore
build/
slides.pdf
EOF

cat << EOF > .prezrc
title = "Presentation Title"
slides = "slides"
print = false

theme = "beige"
printTheme = "beige"
;; black white league sky beige simple serif blood night moon solarized

highlightTheme = "default"
;; https://github.com/isagalaev/highlight.js/tree/master/src/styles

suchNotes = false
skipReveal = false
skipIndex = false
skipUser = false
keepHidden = false
phantomjs = "phantomjs"
dynamicTheme = true
watch = false
subCovers = false
EOF

cat << EOF > slides/01-intro.md
# Presentation Title

Something about the presentation

Glynn Forrest

me@glynnforrest.com
EOF

cat << 'EOF' > slides/02-preamble.md
$background:images/background.png$
$background-size:20%$
$background-position:1% bottom$

## Preamble

```python
@annotate
def hello(args):
    return 'Hello world!'
```

<p class="fragment fade-in">one</p>
<p class="fragment fade-in">two</p>
<p class="fragment fade-in">three</p>

note:

speaker notes here
EOF

cat << EOF > Makefile
.PHONY: build
build:
	prez .

clean:
	rm -rf build
	rm -f slides.pdf

present:
	prez --serve .

pdf:
	prez --print slides.pdf

dev:
	prez --serve --watch .
EOF

make dev
