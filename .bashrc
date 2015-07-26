# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions

source ~/.bash_aliases

OS=$(lsb_release -si)

export TERM=xterm-256color
export PATH=$HOME/Dropbox/vhome/bin:$SMLROOT/bin:$SMLROOT/lib:$HOME/bin:$PATH

# JAVA
export JAVA_HOME=/home/bibou/jdk/jdk1.8.0_45
export PATH=$JAVA_HOME/bin:$PATH

# JTREG
export JT_HOME=$HOME/Projects/openjdk/jtreg
export PRODUCT_HOME=/home/bibou/Projects/openjdk/jdk9/build/linux-x86_64-normal-server-release/jdk
export PATH=$JT_HOME/linux/bin:$PATH

# GRAAL
export PATH=$HOME/Projects/openjdk/graal/mxtool:$PATH

# Haskell
export PATH=$HOME/.cabal/bin:$PATH

# Scala
export SCALA_HOME=/usr/local/share/scala
export PATH=$SCALA_HOME/bin:$PATH

# MorphJ/DelphJ
export mj=/home/bibou/Projects/morphing
export CLASSPATH=$mj/MorphJ/MJBackend/src:$mj/MorphJ/MJBackend/thirdparty/asm-all-3.1.jar:/usr/share/java/junit4.jar

# MLTON
export SML_LIB=/usr/lib64/mlton/sml/basis

# SML/NJ
export SMLROOT=/usr/local/sml
export PATH=$SMLROOT/bin:$PATH

# function parse_git_branch {
#   ref=$(git symbolic-ref HEAD 2> /dev/null) || return
#   echo "["${ref#refs/heads/}"]"
# }
# function parse_svn_branch() {
#   parse_svn_url | sed -e 's#^'"$(parse_svn_repository_root)"'##g' | egrep -o '(tags|branches)/[^/]+|trunk' | egrep -o '[^/]+$' | awk '{print "["$1"]" }'
# }
# function parse_svn_url() {
#   svn info 2>/dev/null | sed -ne 's#^URL: ##p'
# }
# function parse_svn_repository_root() {
#   svn info 2>/dev/null | sed -ne 's#^Repository Root: ##p'
# }
# RED="\[\e[0;31m\]"
# YELLOW="\[\e[0;93m\]"
# GREEN="\[\e[0;32m\]"
# NONE="\[\e[0m\]"
# CYAN="\[\e[1;36m\]"

# PS1="$NONE$GREEN[\w]$NONE $RED\$(parse_git_branch)$NONE \n$CYAN\u$NONE $YELLOW\$ $NONE"

