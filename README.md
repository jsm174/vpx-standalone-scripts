# vpx-standalone-scripts
Table patches for VPX Standalone

Since [Visual Pinball](https://github.com/vpinball/vpinball) Standalone is using the [wine](https://www.winehq.org/) vbscript engine there are some slight [incompatibilities](https://github.com/vpinball/vpinball/blob/10.8.1/standalone/README.md#background). Luckily only a few tables require modifications.

This repository contains the original script, patch and fixed script for tables that do not run properly by default.

## Installation of a patch

Just find the related file with `.vbs` extention in this repisitory. It should have the same file name as the vpx file tou downloaded. Put the patch next to the vpx file and vpinball should automatically pick it up. These script files are sometimes referred to as a sidecar script.

## Creating a patch

1. Start by extracting the script
  ```shell
  VPinballX_GL -extractvbs [tablename.vpx]
  ```
2. Apply the patch, put it in this repository and run `./generate.sh`
3. Create a PR with the changes

*You can also use [vpxtool](https://github.com/francisdb/vpxtool) to extract, apply some automatic fixes and generate patch files.*
