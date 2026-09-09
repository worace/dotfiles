#!/bin/bash
# Shared repo and worktree path resolution for the worktree scripts.
#
# Nothing here names a specific project. The repo to operate on is normally
# discovered from the invoking directory; a per-machine config file supplies a
# default for the callers that run outside any checkout (tmux-workspace at
# login, for example).
#
# Config file — every key optional, `KEY=value` lines, `#` starts a comment,
# and the only expansion applied to a value is a leading `~/` or `$HOME/`:
#
#   ${XDG_CONFIG_HOME:-~/.config}/worktrees/config
#     WORKTREE_MAIN_REPO=~/code/my-repo   # default repo when cwd isn't in one
#     WORKTREE_ROOT=~/worktrees           # holds one subdirectory per repo
#
# Environment variables of the same names win over the config file, and for the
# main repo an explicit environment value also wins over the cwd.
#
# Worktrees live at $WORKTREE_ROOT/<repo-basename>/<branch>.

# Captured at source time so the accessors can tell an explicit environment
# override apart from a value the config file filled in later.
WORKTREE_MAIN_REPO_FROM_ENV="${WORKTREE_MAIN_REPO:-}"

worktree_config_file() {
    printf '%s\n' "${WORKTREE_CONFIG:-${XDG_CONFIG_HOME:-$HOME/.config}/worktrees/config}"
}

# Expand a leading `~/` or `$HOME/` only — enough for the paths these configs
# hold, and it keeps the parser from having to eval anything.
worktree_expand_home() {
    local value="$1"
    case "$value" in
        '~/'*)     printf '%s\n' "$HOME/${value#\~/}" ;;
        '$HOME/'*) printf '%s\n' "$HOME/${value#\$HOME/}" ;;
        *)         printf '%s\n' "$value" ;;
    esac
}

worktree_trim() {
    local value="$1"
    value="${value#"${value%%[![:space:]]*}"}"
    value="${value%"${value##*[![:space:]]}"}"
    printf '%s\n' "$value"
}

# Fill WORKTREE_MAIN_REPO / WORKTREE_ROOT from the config file, but only where
# the environment has not already set them. Unrecognized keys are ignored so
# the file can carry settings for other tools.
worktree_config_load() {
    local file line key value
    file="$(worktree_config_file)"
    [[ -f "$file" ]] || return 0

    while IFS= read -r line || [[ -n "$line" ]]; do
        line="${line%%#*}"
        [[ "$line" == *=* ]] || continue
        key="$(worktree_trim "${line%%=*}")"
        value="$(worktree_trim "${line#*=}")"
        value="${value#[\"\']}"
        value="${value%[\"\']}"
        [[ -n "$value" ]] || continue
        case "$key" in
            WORKTREE_MAIN_REPO)
                [[ -n "${WORKTREE_MAIN_REPO:-}" ]] || WORKTREE_MAIN_REPO="$(worktree_expand_home "$value")" ;;
            WORKTREE_ROOT)
                [[ -n "${WORKTREE_ROOT:-}" ]] || WORKTREE_ROOT="$(worktree_expand_home "$value")" ;;
        esac
    done < "$file"
}

# The main checkout of the repo containing the current directory, or empty when
# the cwd is not inside a repo. A linked worktree resolves to its main
# checkout, not to itself, so `new-worktree` run from one worktree still adds
# the next worktree alongside it.
worktree_repo_from_cwd() {
    local common_dir
    common_dir="$(git rev-parse --path-format=absolute --git-common-dir 2>/dev/null)" || return 0
    if [[ "$(basename "$common_dir")" == ".git" ]]; then
        printf '%s\n' "$(dirname "$common_dir")"
        return 0
    fi
    # Bare repo or an unusual GIT_DIR: fall back to whatever git calls the top.
    git rev-parse --show-toplevel 2>/dev/null || true
}

# The repo to operate on: an explicit environment override, else the cwd's
# repo, else the configured default. Empty when none of the three resolve.
worktree_main_repo() {
    worktree_config_load

    if [[ -n "$WORKTREE_MAIN_REPO_FROM_ENV" ]]; then
        printf '%s\n' "$WORKTREE_MAIN_REPO_FROM_ENV"
        return 0
    fi

    local from_cwd
    from_cwd="$(worktree_repo_from_cwd)"
    if [[ -n "$from_cwd" ]]; then
        printf '%s\n' "$from_cwd"
        return 0
    fi

    printf '%s\n' "${WORKTREE_MAIN_REPO:-}"
}

# The configured default repo, ignoring the cwd entirely. For callers whose cwd
# carries no intent — a login-time session builder is in whatever directory the
# shell happened to start in.
worktree_configured_main_repo() {
    worktree_config_load
    printf '%s\n' "${WORKTREE_MAIN_REPO:-}"
}

worktree_root() {
    worktree_config_load
    printf '%s\n' "${WORKTREE_ROOT:-$HOME/worktrees}"
}

# Where this repo's worktrees live: one directory per repo, named after it.
worktree_base() {
    local repo="$1"
    printf '%s/%s\n' "$(worktree_root)" "$(basename "$repo")"
}

# A repo's bootstrap hook, or empty when it has none. Checked in the repo first
# so a project can own its own setup, then in the config directory for repos
# you can't (or don't want to) add a file to.
worktree_setup_hook() {
    local repo="$1" candidate
    for candidate in \
        "$repo/.worktree-setup" \
        "$(dirname "$(worktree_config_file)")/hooks/$(basename "$repo")"
    do
        if [[ -x "$candidate" ]]; then
            printf '%s\n' "$candidate"
            return 0
        fi
    done
}
