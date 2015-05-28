source ~/.zsh/git-prompt/zshrc.sh

# Attribute codes:
# 00=none 01=bold 04=underscore 05=blink 07=reverse 08=concealed
# Text color codes:
# 30=black 31=red 32=green 33=yellow 34=blue 35=magenta 36=cyan 37=white
# Background color codes:
# 40=black 41=red 42=green 43=yellow 44=blue 45=magenta 46=cyan 47=white

esc="%{"
color="${esc}[;"
Normal="${esc}[0m%}"
escEnd="m%}"
Black=${color}30$escEnd
Red=${color}31$escEnd
Green=${color}32$escEnd
Yellow=${color}33$escEnd
Blue=${color}34$escEnd
Magenta=${color}35$escEnd
Cyan=${color}36$escEnd
White=${color}37$escEnd

if [[ -n $SSH_CONNECTION ]]; then
  Machine="@%B%m%b"
else
  Machine=""
fi

PS1_STANDARD="[$Blue%n$Black$Machine $Blue%B%~$Normal%b]%(#.#.$) "
PS1_ROOT="[${Red}%BROOT%b$Black$Machine $Blue%B%~$Normal%b]%(#.#.$) "
if [[ $USERNAME == root ]] ; then
   PS1=$PS1_ROOT	
else
   PS1=$PS1_STANDARD
fi
export PS1

RPROMPT='%b$(git_super_status)'
unset esc color Normal escEnd Black Red Green Yellow Blue Magenta Cyan White Machine
