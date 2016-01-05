#!/bin/bash
#
# Create a distribution archive from the current source code.
#

set -e
set -u

# Define the contents of the final archive:
readonly ARCHIVE_CONTENTS=("examples/" "lib/" "LICENSE" "Makefile" \
                           "modules/" "README.md" "src/" "test/" \
                           "VERSION")

# Clean the code directory first:
make clean
make -C test distclean-suites

# Create a temporary build directory:
readonly BUILD_NAME="rpe-$(head -n 1 VERSION)"
readonly BUILD_DIR="dist/$BUILD_NAME"

if [[ -d "dist/$BUILD_NAME" ]]; then
    echo "build directory 'dist/$BUILD_NAME' already exists, cleaning" 1>&2
    rm -rf "dist/$BUILD_NAME"
fi
mkdir -p "dist/$BUILD_NAME"

# Copy the required files into the temporary build directory:
for item in ${ARCHIVE_CONTENTS[@]}; do
    cp -r "$item" "dist/$BUILD_NAME"
done

# Create the distribution archive within the dist/ directory:
readonly ARCHIVE_NAME="${BUILD_NAME}.tar.gz"
cd dist
tar cfzv "$ARCHIVE_NAME" --exclude-backups --exclude='.gitignore' "$BUILD_NAME"
rm -rf "$BUILD_NAME"
