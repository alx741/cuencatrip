#!/bin/sh

function init()
{
    tmux new-window -n "yesod" "stack ghci"
}

function reload()
{
    tmux send-keys -t "yesod" ":l DevelMain"
    tmux send-keys -t "yesod" "Enter"
    tmux send-keys -t "yesod" "DevelMain.update"
    tmux send-keys -t "yesod" "Enter"
}

if [[ "$1" == "init" ]];
then
    init
elif [[ "$1" == "reload" ]];
then
    reload
fi
