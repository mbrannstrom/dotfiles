# .zshrc

## Prompt
source ~/.zsh/prompt.sh


## Misc
setopt AutoCD                        # Allow entering directories by giving their path alone

setopt NoListBeep                    # Do not beep on an ambiguous completion!
setopt NoBeep                        # Do I want this? (probably no effect anyway ;P)

setopt NoBGNice                      # Don't you *ever* nice any processes unless I ask you to
setopt NoFlowControl                 # Disable scroll lock via ctrl-s/ctrl-q in shell.
unsetopt BangHist                    # Do not treat ! character specially
setopt RCQuotes                      # Allow '' to signify a single quote within singly quoted strings.
unsetopt AutoRemoveSlash

# Home, end and delete
bindkey "^[[1~" beginning-of-line
bindkey "^[[4~" end-of-line
bindkey "^[[3~" delete-char
bindkey "[W" backward-delete-word  # Ctrl-BackSpace: string("[W")
bindkey "[V" delete-word           # Ctrl-Delete: string("[V")


## History
export HISTFILE=~/.zsh_history
setopt ShareHistory AppendHistory    # Share history between parallel zsh sessions
setopt InteractiveComments           # Allow commenting out lines by prepending '#'
setopt HistIgnoreDups                # Don't record dups in history
setopt HistIgnoreSpace               # Don't record commands preceded with a space

# Automatic history search with arrow keys
bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward
bindkey "^[OA" history-beginning-search-backward
bindkey "^[OB" history-beginning-search-forward


## Completion and globbing
setopt AutoList
unsetopt MenuComplete
unsetopt AutoMenu
setopt NumericGlobSort               # Sort numerical filenames are match by glob pattern, sort them numerically
setopt ExtendedGlob                  # Allow cool globbing (negating with ^, for example)
setopt GlobComplete                  # Complete globbing patterns


## Environment variables and aliases
source ~/.alias
source ~/.environment
source ~/.setenv                     # OS X environment variables for apps not launched from a terminal
