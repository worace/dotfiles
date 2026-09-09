#!/usr/bin/env python3
"""Copy the current Zed line as a rich-text GitHub permalink."""

from __future__ import annotations

import re
import subprocess
import sys
import tempfile
from pathlib import Path


def git(*args: str, cwd: Path) -> str:
    return subprocess.check_output(["/usr/bin/git", *args], cwd=cwd, text=True).strip()


def github_base_url(remote: str) -> str:
    remote = remote.removesuffix(".git")
    if match := re.fullmatch(r"(?:git@|ssh://git@)([^:/]+)[:/]([^\s]+)", remote):
        return f"https://{match.group(1)}/{match.group(2)}"
    if match := re.fullmatch(r"https?://([^/]+)(/.*)", remote):
        return f"https://{match.group(1)}{match.group(2)}"
    raise ValueError(f"Couldn't interpret Git remote: {remote}")


def line_label(line: str) -> str:
    for pattern in (
        r"^\s*(?:export\s+)?(?:default\s+)?(?:abstract\s+)?class\s+([A-Za-z_$][\w$]*)",
        r"^\s*(?:export\s+)?(?:async\s+)?function\s+([A-Za-z_$][\w$]*)",
        r"^\s*(?:async\s+)?def\s+([A-Za-z_]\w*)\s*\(",
        r"^\s*(?:pub(?:\([^)]*\))?\s+)?(?:async\s+)?fn\s+([A-Za-z_]\w*)\s*\(",
    ):
        if match := re.match(pattern, line):
            return match.group(1)

    compact = "".join(line.split())
    if not compact:
        raise ValueError("The current line is blank")
    return compact[:20]


def rtf_escape(value: str) -> str:
    escaped: list[str] = []
    for character in value:
        if character in "\\{}":
            escaped.append("\\" + character)
        elif ord(character) > 127:
            escaped.append(f"\\u{ord(character)}?")
        else:
            escaped.append(character)
    return "".join(escaped)


def copy_rtf_link(label: str, permalink: str) -> None:
    rtf = (
        r"{\rtf1\ansi\deff0{\fonttbl{\f0 Helvetica;}}"
        rf'{{\field{{\*\fldinst{{HYPERLINK "{rtf_escape(permalink)}"}}}}'
        rf"{{\fldrslt{{\ul\cf1 {rtf_escape(label)}}}}}}}}}"
    )
    with tempfile.NamedTemporaryFile(mode="w", suffix=".rtf", encoding="utf-8") as file:
        file.write(rtf)
        file.flush()
        subprocess.run(
            [
                "/usr/bin/osascript",
                "-e",
                'use scripting additions\non run argv\nset rtfData to (read (POSIX file (item 1 of argv)) as «class RTF »)\nset the clipboard to {Unicode text:(item 2 of argv), «class RTF »:rtfData}\nend run',
                file.name,
                label,
            ],
            check=True,
        )


def main() -> None:
    if len(sys.argv) != 4:
        raise ValueError("Usage: code-link-clipboard.py FILE ROW WORKTREE_ROOT")

    file_path = Path(sys.argv[1]).resolve()
    row = int(sys.argv[2])
    if row <= 0:
        raise ValueError("Zed did not provide a valid line number")
    lines = file_path.read_text(encoding="utf-8").splitlines()
    line = lines[row - 1]
    root = Path(git("rev-parse", "--show-toplevel", cwd=Path(sys.argv[3]))).resolve()
    remote = git("remote", "get-url", "origin", cwd=root)
    commit = git("rev-parse", "HEAD", cwd=root)
    permalink = f"{github_base_url(remote)}/blob/{commit}/{file_path.relative_to(root)}#L{row}"

    copy_rtf_link(line_label(line), permalink)
    print(f"Copied linked text: {line_label(line)}")


if __name__ == "__main__":
    try:
        main()
    except (OSError, subprocess.CalledProcessError, ValueError, IndexError) as error:
        print(f"Code-link clipboard error: {error}", file=sys.stderr)
        raise SystemExit(1) from error
