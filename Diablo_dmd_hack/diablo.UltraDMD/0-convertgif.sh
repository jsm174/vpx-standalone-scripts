#!/bin/bash
for i in *.wmv ; do ffmpeg -i "$i" "${i%.*}.gif" ; done

