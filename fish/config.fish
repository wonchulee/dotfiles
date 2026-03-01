# fish/config.fish — OS-aware configuration

# -- common aliases --------------------------------------------------------
alias cat='bat'
alias ls='eza'
alias tf='terraform'
alias k='kubectl'

# -- common environment ----------------------------------------------------
set -gx EDITOR vim

set -gx NPM_PACKAGES_BIN $HOME/.npm-packages/bin
set -gx PATH $HOME/.local/bin $NPM_PACKAGES_BIN $HOME/.cargo/bin $HOME/go/bin $PATH
set -gx MANPATH $MANPATH $HOME/.npm-packages/share/man

# -- OS-specific -----------------------------------------------------------
switch (uname)
    case Darwin
        # Homebrew
        eval (/opt/homebrew/bin/brew shellenv)

        # Python user-base (dynamic)
        if command -q python3
            set -l pybase (python3 -m site --user-base 2>/dev/null)
            if test -n "$pybase"
                set -gx PATH "$pybase/bin" $PATH
            end
        end

        # ffmpeg (dynamic)
        if command -q brew
            set -l ffmpeg_prefix (brew --prefix ffmpeg@6 2>/dev/null)
            if test -n "$ffmpeg_prefix" -a -d "$ffmpeg_prefix"
                set -gx PATH "$ffmpeg_prefix/bin" $PATH
                set -gx LD_LIBRARY_PATH "$ffmpeg_prefix/lib" $LD_LIBRARY_PATH
                set -gx PKG_CONFIG_PATH "$ffmpeg_prefix/lib/pkgconfig" $PKG_CONFIG_PATH
            end
        end

        set -gx GST_PLUGIN_PATH_1_0 /opt/homebrew/lib/gstreamer-1.0/

        # Docker Desktop
        test -f $HOME/.docker/init-fish.sh; and source $HOME/.docker/init-fish.sh; or true

        # Rancher Desktop
        if test -d "$HOME/.rd/bin"
            set --export --prepend PATH "$HOME/.rd/bin"
        end

        # iTerm2 shell integration
        test -e $HOME/.iterm2_shell_integration.fish; and source $HOME/.iterm2_shell_integration.fish; or true

    case Linux
        set -gx GTK_IM_MODULE ibus
        set -gx LD_LIBRARY_PATH $HOME/.local/lib64 $HOME/.local/lib $LD_LIBRARY_PATH
        set -gx PKG_CONFIG_PATH $HOME/.local/lib64/pkgconfig $HOME/.local/lib/pkgconfig $PKG_CONFIG_PATH
end

# -- shared tools (after OS PATH setup) ------------------------------------
# starship prompt
if command -q starship
    starship init fish | source
end

# volta
set -gx VOLTA_HOME "$HOME/.volta"
set -gx PATH "$VOLTA_HOME/bin" $PATH
