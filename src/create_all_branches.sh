#!/bin/sh

set -euf

remotes='origin
sync'

if [ "${1:-}" = 'reset_upstream' ]; then
    echo "Resetting local branch upstreams."
    for lb in $(git rev-parse --abbrev-ref --branches='*'); do
        r=0
        u="$(git rev-parse --abbrev-ref "$lb@{upstream}" 2>/dev/null)" || r=$?
        if [ $r = 0 ]; then
            u_remote="${u%/*}"
            # Reset only upstreams, which i will configure later.
            if echo "$remotes" | grep -q $u_remote; then
                git branch --unset-upstream "$lb"
                echo "Branch '$lb' upstream '$u' was unset."
            fi
        fi
    done
fi

for remote in $remotes; do
    echo "Creating branches from '$remote' remote."
    for rb in $(git rev-parse --abbrev-ref --remotes="$remote/*" | sort -u); do
        lb="${rb##*/}"
        if git rev-parse --quiet --verify "$lb" >/dev/null ; then
            r=0
            u="$(git rev-parse --abbrev-ref "$lb@{upstream}" 2>/dev/null)" || r=$?
            if [ $r -ne 0 ]; then
                git branch --set-upstream-to="$rb" "$lb"
            elif [ "$u" != "$rb" ]; then
                echo "W: Branch '$lb' has already configured upstream '$u'."
            fi
        else
            git branch --track "$lb" "$rb"
        fi
    done
done
