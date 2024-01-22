# vpx-standalone-scripts
Table patches for VPX Standalone

[Visual Pinball](https://github.com/vpinball/vpinball) Standalone is using the [vbscript engine](https://gitlab.winehq.org/wine/wine/-/tree/master/dlls/vbscript?ref_type=heads) from [Wine](https://www.winehq.org/) and there are some slight [incompatibilities](https://github.com/vpinball/vpinball/blob/10.8.1/standalone/README.md#background). Luckily only a few tables require modifications.

This repository contains the original script, patch, and fixed script for tables that do not run properly by default.

## Installation of a patch

Just find the related file with `.vbs` extention in this repository. It should have the same file name as the vpx file you downloaded. Put the patch next to the vpx file and vpinball will automatically pick it up. These script files are sometimes referred to as a sidecar script.

## Creating a patch

1. Start by extracting the script
  ```shell
  VPinballX_GL -extractvbs [tablename.vpx]
  ```
2. Apply the patch, put it in this repository and run `./generate.sh`
3. Create a PR with the changes

*You can also use [vpxtool](https://github.com/francisdb/vpxtool) to extract, apply some automatic fixes and generate patch files.*
