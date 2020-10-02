#!/bin/sh

set -euf

repo_url="$1"
sync_url="$2"
repo="$(basename "${repo_url##*:}" .git)"
tmp_repo="git_$repo"

echo "Will use '$repo_url' as 'origin' remote."
echo "Will use '$sync_url' as 'sync' remote (_mirroring_ everything)."
read -p "Continue? " x
if [ "$x" != 'y' ]; then
    echo "Exit."
    exit 0
fi

if [ ! -d "$repo" ]; then
    git clone --mirror "$sync_url" "$tmp_repo"
    mkdir "$repo"
    mv "$tmp_repo" "$repo/.git"
    cd "$repo"
    git config --bool core.bare false
    git config --bool core.logallrefupdates true
    git checkout master
    git remote rename origin sync
    git remote add origin "$repo_url"
    git remote update origin
    git push -u origin :
else
    echo "Repository directory '$repo' already exists."
    exit 0
fi
