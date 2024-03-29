#!/bin/bash
# used to select/delete clipboard history from xmonad.
export FZF_DEFAULT_OPTS='--layout=reverse --border --inline-info'

case "$1" in
    "sel") 
        recid=$(clipc --list | fzf | awk '{print $1}')
        [[ -n $recid ]] \
            && clipc --select $recid \
            && xclip -o -selection clipboard | xsel --clipboard
                    ;;
    "del")
        recid=$(clipc --list | fzf -m | awk '{print $1}')
        for i in $recid; do clipc --delete $i; done
                    ;;
esac
