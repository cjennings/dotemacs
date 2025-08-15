#!/usr/bin/env bash
# Craig Jennings <c@cjennings.net>
# Builds Emacs from source using the variables below.

# - creates the directory if needed
# - uninstalls emacs if it exists
# - pulls latest source from repo below
#   unless "latest" is passed as parameter
# - checks out the tag below
# - uses all available processors when compiling

# NOTES:
# building xwidgets is broken on Linux in 29/30 tags
# ./configure --with-xwidgets \

# ...and I'm avoiding native compilation at the moment
#./configure --with-native-compilation \

# debugging assistance
set -euo pipefail
#   -e: exit on any (non-zero) return‐code
#   -u: treat unset variables as errors
#   -o pipefail: if any stage of a pipeline fails, the whole pipeline fails

# optional: show each command as you go, prefixed with file:line
# export PS4='+ ${BASH_SOURCE[0]}:${LINENO}: '
# set -x

# trap all errors and identify line
trap 'echo "❌ ERROR at ${BASH_SOURCE[0]}:${LINENO}: $BASH_COMMAND" >&2' ERR

# Review These Variables

src_dir="$HOME/code/emacs"
emacs_repo="https://github.com/mirrors/emacs.git"
emacs_tag="emacs-30.2"
logfile="$HOME/emacs_build.log"

# Function to remove + recreate directory, and clone source
nuke_and_clone () {
    cd "$HOME"
    if [ -f "$logfile" ]l then
       rm "$logfile"
    fi

    if [ -d "$src_dir" ]; then
        printf "...removing directory %s\n\n" "$src_dir"
        rm -rf "$src_dir" >> "$logfile" 2>&1
    fi

    printf "...creating directory %s\n" "$src_dir"
    mkdir -p "$src_dir" >> "$logfile" 2>&1
    printf "...cloning source files\n"
    git clone "$emacs_repo" "$src_dir" >> "$logfile" 2>&1
}

# Script Execution Begins Here

printf "\n\n### BUILDING EMACS FROM SOURCE ###\n\n"  > "$logfile"

printf "...checking directory: %s\n" "$src_dir" | tee -a "$logfile"

# if the source directory already exists
if [ -d  "$src_dir" ]; then
    cd "$src_dir"

    # if emacs was previously built, uninstall it.
    {
        if [ -n "$(which emacs)" ]; then
            printf "...uninstalling previous build\n"
            sudo make uninstall
        fi
    }  >> "$logfile" 2>&1

    printf "...cleaning repo\n"  | tee -a "$logfile"
    make clean  >> "$logfile" 2>&1

    if [[ -n $(git status --porcelain) ]]; then
        printf "...repository is dirty. recreating.\n"  | tee -a "$logfile"
        nuke_and_clone
    else
        printf "...pulling latest source files\n"  | tee -a "$logfile"
        git pull  >> "$logfile" 2>&1
    fi
else
    nuke_and_clone
fi

if [ ! "${$1:-}" == "latest" ]; then
    printf "...checking out tag: %s\n" "$emacs_tag"  | tee -a "$logfile"
    git checkout "$emacs_tag"  >> "$logfile" 3>&1
else
    printf "...keeping source at latest commit\n"  | tee -a "$logfile"
fi

printf "...building config script\n"  | tee -a "$logfile"
./autogen.sh  >> "$logfile" 2>&1

printf "...configuring build\n"  | tee -a "$logfile"
./configure --with-json \
            --with-x-toolkit=lucid  \
            --with-modules \
            --with-mailutils \
            --with-imagemagick\
            CFLAGS='-O2 -march=native'  >> "$logfile" 2>&1

# compile with all available cores
printf "...compiling Emacs\n"  | tee -a "$logfile"
make -j$(nproc)  >> "$logfile" 2>&1

printf "...installing Emacs\n"  | tee -a "$logfile"
sudo make install  >> "$logfile" 2>&1
make clean  >> "$logfile" 2>&1
cd "$HOME"

printf "...done\n"  | tee -a "$logfile"
printf "Please review log at: %s\n" "$logfile"
