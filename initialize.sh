#!/bin/bash

set -e

while read package; do
    stow -t ~ -R "$package"
    echo "stowed package $package"
done < packages.txt
