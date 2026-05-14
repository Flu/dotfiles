function theme_preexec {
  setopt local_options extended_glob
  if [[ "$TERM" = "screen" ]]; then
    local CMD=${1[(wr)^(*=*|sudo|-*)]}
    echo -n "\ek$CMD\e\\"
  fi
}

autoload -U add-zsh-hook
add-zsh-hook preexec theme_preexec

setopt prompt_subst

autoload zsh/terminfo

# Git prompt config
ZSH_THEME_GIT_PROMPT_PREFIX=" on %{$fg[cyan]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=""
ZSH_THEME_GIT_PROMPT_CLEAN=""

ZSH_THEME_GIT_PROMPT_ADDED="%{$fg[green]%} ✚%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_MODIFIED="%{$fg[yellow]%} ✹%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DELETED="%{$fg[red]%} ✖%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_RENAMED="%{$fg[magenta]%} ➜%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_UNMERGED="%{$fg[yellow]%} ═%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[cyan]%} ✭%{$reset_color%}"

# Virtualenv / conda shown without the default prefix, we'll inline them
ZSH_THEME_VIRTUAL_ENV_PROMPT_PREFIX=" | %{$fg[green]%}"
ZSH_THEME_VIRTUAL_ENV_PROMPT_SUFFIX="%{$reset_color%}"

# Titlebar
case $TERM in
  xterm*)
    PR_TITLEBAR=$'%{\e]0;%(!.-=*[ROOT]*=- | .)%n@%m:%~ | %y\a%}'
    ;;
  screen)
    PR_TITLEBAR=$'%{\e_screen \005 (\005t) | %(!.-=[ROOT]=- | .)%n@%m:%~ | %y\e\\%}'
    ;;
  *)
    PR_TITLEBAR=""
    ;;
esac

if [[ "$TERM" = "screen" ]]; then
  PR_STITLE=$'%{\ekzsh\e\\%}'
else
  PR_STITLE=""
fi

# Build the env info segment (venv + conda, with separator)
_env_info() {
  local info=""
  local venv="$(virtualenv_prompt_info 2>/dev/null)"
  local conda="$(conda_prompt_info 2>/dev/null)"
  [[ -n "$venv"  ]] && info+=" | %{$fg[green]%}${venv}%{$reset_color%}"
  [[ -n "$conda" ]] && info+=" | %{$fg[green]%}${conda}%{$reset_color%}"
  echo -n "$info"
}

local c_orange=$(printf "\033[38;5;215m")
local c_red=$(printf "\033[38;5;209m")
local c3=$(printf "\033[38;5;203m")
# Line 1: user@host:/path on branch <gitstatus> [envs] at HH:MM:SS (exitcode)
# Line 2: λ
PROMPT='${PR_STITLE}${(e)PR_TITLEBAR}\
%{$c_orange%}%n@%{$reset_color%}\
%{$c3%}%m%{$reset_color%}\
%{$c3%}:%{$reset_color%}\
%{$c_red%}%~%{$reset_color%}\
$(git_prompt_info)\
$(git_prompt_status)\
$(_env_info)\
 %{$fg[green]%}[%{$reset_color%} %{$fg[orange]%}%D{%H:%M:%S}%{$reset_color%} %{$fg[green]%}]%{$reset_color%}\
 %{$fg_bold[red]%}(%?)%{$reset_color%}
%{$fg[white]%}λ%{$reset_color%} '

RPROMPT=''
PS2='%{$fg[red]%}λ%{$reset_color%} '
