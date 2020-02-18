# .zshrc

# enable the default zsh completions!
autoload -Uz compinit && compinit

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


# Java
function removeFromPath() {
    export PATH=$(echo $PATH | sed -E -e "s;:$1;;" -e "s;$1:?;;")
}
if [ -f "/usr/libexec/java_home" ]; then # Java on MacOS
    function setjdk() {
        if [ $# -ne 0 ]; then
            export JDK=`/usr/libexec/java_home -v $@`
            removeFromPath '/System/Library/Frameworks/JavaVM.framework/Home/bin'
            if [ -n "${JAVA_HOME+x}" ]; then
                removeFromPath $JAVA_HOME
            fi
            export JAVA_HOME="$JDK"
            export PATH=$JAVA_HOME/bin:$PATH
        else
            /usr/libexec/java_home -V 2>&1 | grep , | sed -e 's/,.*//' 
        fi
    }
elif [ -d "/usr/lib/jvm" ]; then # Java on Debian/Ubuntu
    function setjdk() {
        if [ $# -ne 0 ]; then
            JDK=`setjdk | grep $1 | head -n 1`
            if [[ "$JDK" == "" ]]; then
                echo "Unable to find any JVMs matching version '$1'"
                return 1
            else
                if [ -n "${JAVA_HOME+x}" ]; then
                    removeFromPath $JAVA_HOME
                fi
                export JAVA_HOME="/usr/lib/jvm/$JDK"
                export PATH=$JAVA_HOME/bin:$PATH
            fi
        else
            update-alternatives --list java | sed -r -e 's;/usr/lib/jvm/([^/]*)/.*;\1;'
        fi
    }
fi

## Environment variables and aliases
source ~/.alias
source ~/.environment
source ~/.setenv                     # OS X environment variables for apps not launched from a terminal

# Google Cloud SDK
if [ -d "/opt/google-cloud-sdk" ]; then
    source /opt/google-cloud-sdk/path.zsh.inc
    source /opt/google-cloud-sdk/completion.zsh.inc
fi
