"""
Sync declared Quarto resources into docs/, the folder GitHub Pages serves.

Pages builds this site from main:/docs (legacy mode, no workflow), so a file
under lectures/ or assignments/ only goes live once its docs/ copy is updated.
Quarto does that copy as part of a full render; this script does only the copy,
so swapping one PDF doesn't need a rebuild of every page.

  python3 sync_site_resources.py           # copy stale resources into docs/
  python3 sync_site_resources.py --check   # report only; exit 1 if any are stale

Synced paths are printed to stdout, one per line, for the pre-commit hook to
stage. Human-readable notes go to stderr.
"""

from pathlib import Path
import argparse
import filecmp
import shutil
import sys

import yaml

ROOT = Path(__file__).resolve().parent
DOCS = ROOT / "docs"


def declared_resources():
    """Every file matched by project.resources in _quarto.yml, relative to ROOT."""
    config = yaml.safe_load((ROOT / "_quarto.yml").read_text())
    patterns = config.get("project", {}).get("resources") or []
    files = []
    for pattern in patterns:
        for path in sorted(ROOT.glob(pattern)):
            if path.is_file():
                files.append(path.relative_to(ROOT))
    return files


def stale(resources):
    """Resources whose docs/ copy is missing or differs byte-for-byte."""
    out = []
    for rel in resources:
        copy = DOCS / rel
        if not copy.exists() or not filecmp.cmp(ROOT / rel, copy, shallow=False):
            out.append(rel)
    return out


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true",
                        help="report stale copies without writing; exit 1 if any")
    args = parser.parse_args()

    resources = declared_resources()
    pending = stale(resources)

    if not pending:
        print(f"docs/ is in step with all {len(resources)} declared resources.",
              file=sys.stderr)
        return 0

    for rel in pending:
        if args.check:
            print(f"stale: docs/{rel}", file=sys.stderr)
        else:
            copy = DOCS / rel
            copy.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(ROOT / rel, copy)
            print(f"docs/{rel}")

    verb = "stale" if args.check else "synced"
    print(f"{len(pending)} of {len(resources)} resources {verb}.", file=sys.stderr)
    return 1 if args.check else 0


if __name__ == "__main__":
    sys.exit(main())
