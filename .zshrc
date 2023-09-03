export ZSH="$HOME/.oh-my-zsh"

local plugins_home="$ZSH/custom/plugins"

function _clone {
    local repo_url="$1"
    local repo_path="$2"

    echo -n "cloning $repo_url ... "
    git clone "$repo_url" "$repo_path" --quiet
    echo "done."
}

plugins=(
	git 
	docker 
	nvm 
	kubectl 
	zsh-autosuggestions 
	zsh-nvm 
	zsh-sdkman
)

if [ ! -d "$ZSH" ]; then
    _clone "https://github.com/ohmyzsh/ohmyzsh" "$ZSH"
    _clone "https://github.com/zsh-users/zsh-autosuggestions" "$plugins_home/zsh-autosuggestions"
    _clone "https://github.com/lukechilds/zsh-nvm" "$plugins_home/zsh-nvm"
    _clone "https://github.com/matthieusb/zsh-sdkman" "$plugins_home/zsh-sdkman"
fi

source $ZSH/oh-my-zsh.sh

alias ll="ls -lah --group-directories-first"
alias k="kubectl"
alias h="helm"

HISTSIZE=100000000
SAVEHIST=$HISTSIZE
setopt hist_ignore_space

# theme variables, keep it here
local ret_status="%(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}➜ %s)"
local separator="%{$reset_color%}•"
PROMPT=$'%{$fg[green]%}%n@%m ${separator} %{$fg[cyan]%}%~%{$reset_color%}$(git_prompt_info) ${separator} %{$fg[grey]%}%D{%H:%M:%S}
${ret_status} %{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX=" ${separator} %{$fg_bold[blue]%} %{$fg[purple]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[red]%}✗"
ZSH_THEME_GIT_PROMPT_CLEAN=""
