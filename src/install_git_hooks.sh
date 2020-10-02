#!/bin/sh

# Based on https://stackoverflow.com/questions/3462955/putting-git-hooks-into-repository (https://stackoverflow.com/a/3464399) .

set -euf

hook_names="applypatch-msg pre-applypatch post-applypatch pre-commit prepare-commit-msg commit-msg post-commit pre-rebase post-checkout post-merge pre-receive update post-receive post-update pre-auto-gc"
# assuming the script is in a bin directory, one level into the repo
git_toplevel="$(git rev-parse --show-toplevel)"
git_hook_dir="$(git rev-parse --git-dir)/hooks"
hook_wrapper="$git_hook_dir/hooks-wrapper.sh"
repo_hook_dir="$git_toplevel/git_hooks"

for hook_name in $hook_names; do
    hook="$git_hook_dir/$hook_name"
    # If the hook already exists, is executable, and is not a symlink
    if [ ! -h "$hook" -a -f "$hook" ]; then
        mv -v "$hook" "$hook.local"
    fi
    # create the symlink, overwriting the file if it exists
    # probably the only way this would happen is if you're using an old version of git
    # -- back when the sample hooks were not executable, instead of being named ____.sample
    ln -v -s -f hooks-wrapper.sh "$hook"
done

cat >"$hook_wrapper" <<EOF
#!/bin/sh

set -euf

local_hook="./\$0.local"
if [ -x "\$local_hook" ]; then
    "\$local_hook" "\$@" || exit \$?
fi

repo_hook="$repo_hook_dir/\$(basename \$0)"
if [ -x "\$repo_hook" ]; then
    "\$repo_hook" "\$@" || exit \$?
fi
EOF
chmod u+x "$hook_wrapper"
