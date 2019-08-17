#! /bin/bash

# This should be run from a folder that contains four folders:
#     anteform-linux/  anteform-ms-win/  anteform-osx/ anteform-raspi/
# that each contain the contents of the PICO-8 binary export for the
# appropriate platform plus a docs folder with all supported Anteform
# Manual formats.

butler push anteform-linux feneric/anteform:anteform-linux --userversion 1.0.0
butler push anteform-osx feneric/anteform:anteform-osx --userversion 1.0.0
butler push anteform-ms-win feneric/anteform:anteform-ms-win --userversion 1.0.0
butler push anteform-raspi feneric/anteform:anteform-raspi --userversion 1.0.0
butler status feneric/anteform:anteform-linux
butler status feneric/anteform:anteform-osx
butler status feneric/anteform:anteform-ms-win
butler status feneric/anteform:anteform-raspi

