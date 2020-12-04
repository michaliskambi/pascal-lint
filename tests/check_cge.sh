#!/bin/bash
set -eu

# Run pascal-lint over Castle Game Engine source code.
# Use it from parent dir, i.e. run "tests/check_cge.sh".

FILE_LIST="`find \"$CASTLE_ENGINE_PATH\" -iname \"*.pas\" -or -iname \"*.lpr\"`"
# FILE_LIST="`find $HOME/sources/cat-astrophe-games/escape_universe/trunk/ -iname \"*.pas\" -or -iname \"*.lpr\"`"

./pascal-lint \
  --verbose \
  -Mobjfpc \
  -Sh \
  -Sm \
  -Sc \
  -Si \
  -Fi"$CASTLE_ENGINE_PATH"src/base/ \
  -Fi"$CASTLE_ENGINE_PATH"src/base/opengl/ \
  -Fi"$CASTLE_ENGINE_PATH"src/base/opengl/glsl/ \
  -Fi"$CASTLE_ENGINE_PATH"src/fonts/ \
  -Fi"$CASTLE_ENGINE_PATH"src/fonts/opengl/ \
  -Fi"$CASTLE_ENGINE_PATH"src/opengl/glsl/ \
  -Fi"$CASTLE_ENGINE_PATH"src/window/ \
  -Fi"$CASTLE_ENGINE_PATH"src/window/gtk/ \
  -Fi"$CASTLE_ENGINE_PATH"src/images/ \
  -Fi"$CASTLE_ENGINE_PATH"src/images/opengl/ \
  -Fi"$CASTLE_ENGINE_PATH"src/images/opengl/glsl/ \
  -Fi"$CASTLE_ENGINE_PATH"src/3d/ \
  -Fi"$CASTLE_ENGINE_PATH"src/3d/opengl/ \
  -Fi"$CASTLE_ENGINE_PATH"src/x3d/ \
  -Fi"$CASTLE_ENGINE_PATH"src/x3d/teapot_data/ \
  -Fi"$CASTLE_ENGINE_PATH"src/x3d/opengl/ \
  -Fi"$CASTLE_ENGINE_PATH"src/x3d/opengl/glsl/ \
  -Fi"$CASTLE_ENGINE_PATH"src/audio/ \
  -Fi"$CASTLE_ENGINE_PATH"src/net/ \
  -Fi"$CASTLE_ENGINE_PATH"src/castlescript/ \
  -Fi"$CASTLE_ENGINE_PATH"src/castlescript/opengl/ \
  -Fi"$CASTLE_ENGINE_PATH"src/ui/ \
  -Fi"$CASTLE_ENGINE_PATH"src/ui/opengl/ \
  -Fi"$CASTLE_ENGINE_PATH"src/game/ \
  -Fi"$CASTLE_ENGINE_PATH"src/services/ \
  -Fi"$CASTLE_ENGINE_PATH"src/base/unix/ \
  -Fi"$CASTLE_ENGINE_PATH"src/opengl/unix/ \
  -Fi"$CASTLE_ENGINE_PATH"src/window/unix/ \
  -Fi"$CASTLE_ENGINE_PATH"src/base/windows/ \
  -Fi"$CASTLE_ENGINE_PATH"src/opengl/windows/ \
  -Fi"$CASTLE_ENGINE_PATH"src/window/windows/ \
  -Fi"$CASTLE_ENGINE_PATH"src/ui/windows/ \
  -Fi"$CASTLE_ENGINE_PATH"src/base/android/ \
  -Fi"$CASTLE_ENGINE_PATH"src/base/ios/ \
  $FILE_LIST
