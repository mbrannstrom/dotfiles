#!/bin/zsh

# Resolve symlinks
if [[ ( "$1" == "") ]]
then
    configname=$(hostname -s)
else
    configname=$1
fi
cd ${0:h}
configdir=`pwd`
configdir=${configdir#~/}

# CreateLink (from to)
# Creates a link from $from to $to,
# removing the old link, or moving the old
# file to $from.old
function CreateLink ()
{
	from=~/$1
	from_pretty=\~/$1
	
	if [[ ( "$2" == "" ) ]] 
	then
           arg2=$1
        else
	   arg2=$2
	fi

	to=~/$configdir/$1
	to_pretty=\~/$configdir/$arg2

	if [[ -a $to.$configname ]] 
	then
	    to=$to.$configname
            to_pretty=$to_pretty.$configname
	fi

	echo
	echo Creating a link from $from_pretty to $to_pretty
	if [[ $to == $(readlink $from) ]] 
        then
		echo "Link already exists and is correct."
		return
	fi
	if [[ -h $from ]] 
        then
		echo -n $from_pretty exists as a link to $(readlink $from), remove it'? [Y/n]'
		read -n 1 answer
		if [[ ! $answer == '
' ]] 
		then
		    echo
                fi
		if [[ $answer == n || $answer == N ]] 
                then
		    return
		fi
		echo Ok, removing...
		rm $from
	fi
	if [[ ( -f $from ) && ! ( -h $from ) ]] 
        then
		echo -n $from_pretty exists as a file, rename to $from_pretty.old'? [Y/n]'
		read -n 1 answer
		if [[ ! $answer == '
' ]] 
		then
		    echo
                fi
		if [[ $answer == n || $answer == N ]] 
                then
		    return
		fi
		echo Ok, renaming...
		mv $from $from.old
	fi
	if [[ ( -d $from ) && ! ( -h $from ) ]] 
        then
		echo -n $from_pretty exists as a directory, rename to $from_pretty.old'? [Y/n]'
		read -n 1 answer
		if [[ ! $answer == '
' ]] 
		then
		    echo
                fi
		if [[ $answer == n || $answer == N ]] 
                then
		    return
		fi
		echo Ok, renaming...
		mv $from $from.old
	fi
	echo Creating the link...
	ln -s $to $from
}


function CreateDir ()
{
	dir=~/$1
	dir_pretty=\~/$1
	
	echo
	echo Creating directory $dir_pretty
	if [[ ( -d $dir ) ]] 
        then
	    echo -n "Directory already exists."
	    return
	fi
	if [[ ( -f $dir ) && ! ( -d $dir ) ]] 
        then
		echo -n $dir_pretty exists as a file, rename to $dir_pretty.old'? [Y/n]'
		read -n 1 answer
		if [[ ! $answer == '
' ]] 
		then
		    echo
                fi
		if [[ $answer == n || $answer == N ]] 
		then
		    return
		fi
		echo Ok, renaming...
		mv $dir $dir.old
	fi
	echo Creating the directory...
	mkdir $dir
}

echo CONFIGNAME=$configname

CreateLink .environment 
CreateLink .setenv
CreateLink .alias 
CreateLink .zshrc 
CreateLink .zprofile 
CreateLink .zlogin
CreateDir  .zsh
CreateLink .zsh/prompt.sh
CreateLink .zsh/zsh-git-prompt
CreateLink .cvsrc
CreateLink .gitconfig
CreateLink .inetrc

CreateLink .emacs
CreateDir  .emacs.d
CreateLink .emacs.d/environment.el
CreateLink .emacs.d/abbrev.el
CreateLink .emacs.d/blinkprotocol
CreateLink .emacs.d/markdown
CreateLink .emacs.d/groovy-emacs-modes
CreateLink .emacs.d/yaml-mode
CreateLink .emacs.d/protobuf-mode

CreateDir  bin
CreateLink bin/date2stamp
CreateLink bin/stamp2date
CreateLink bin/expand_inplace
CreateLink bin/emacs
CreateLink bin/gw

