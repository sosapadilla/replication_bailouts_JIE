#!/usr/bin/env python3
"""
cleanup_private_comments.py

Removes residual "private" / dev comments from the Fortran source files of the
submission package for "Optimal Bailouts in Banking and Sovereign Crises".

The patterns removed are inherited from a much older Sosa-Padilla 2015 code
base (Argentina-calibrated dissertation code, internal cluster workflow notes,
renamed parameter explanations, etc.) and would be confusing to a reviewer or
replicator who isn't aware of the code history.

USAGE

    # Dry-run (default; prints what would change, no files touched):
    python3 cleanup_private_comments.py /path/to/Replication

    # Actually apply the edits:
    python3 cleanup_private_comments.py /path/to/Replication --apply

    # Apply, and keep a .bak copy of every modified file:
    python3 cleanup_private_comments.py /path/to/Replication --apply --backup

WHAT IT REMOVES

    Pattern 1: Argentina/JMP attribution block (3 lines)
    Pattern 2: Stale parameter-name notes (theta=, lambda, etc.) (4 lines)
    Pattern 3: gamma_firm / gov_spending / m calibration block (3 lines)
    Pattern 4a: CLUSTER_NOTE "save results" block (2 lines)
    Pattern 4b: CLUSTER_NOTE "indicator_external" line (1 line)
    Pattern 5: Old "Sovereign Defaults and Banking Crises" header
               (only in Tables-section_C2/bailouts_optimal_mu05_nobailout.f90)
"""

import argparse
import re
import shutil
import sys
from pathlib import Path


# Each pattern is a compiled regex matched against the full file text in
# MULTILINE mode. Patterns swallow the trailing newline so the file doesn't
# accumulate blank lines after deletion.

PATTERNS = [
    (
        "P1: Argentina/JMP attribution block",
        re.compile(
            r"^!Argentina \(1990-2010\) -- From Cesar's JMP[ \t]*\r?\n"
            r"^!Rho[ \t]+= 0\.7631[ \t]*\r?\n"
            r"^!Std eps[ \t]+= 0\.0262[ \t]*\r?\n",
            re.MULTILINE,
        ),
    ),
    (
        "P2: Stale theta/lambda parameter notes",
        re.compile(
            r"^! theta= weight of the hh-value function into the planner's objective function\.[ \t]*\r?\n"
            r"^! theta==1 means only hh is relevant; theta==0 means only banker is relevant\.[ \t]*\r?\n"
            r"^! prob_excxlusion_ends -- re-entry probability[ \t]*\r?\n"
            r"^! lambda -- exogenous cost of default[ \t]*\r?\n",
            re.MULTILINE,
        ),
    ),
    (
        "P3: gamma_firm/gov_spending/m calibration block",
        re.compile(
            r"^!gamma_firm = 0\.52 \(average from full sample, look at argentina_data\.xls\)[ \t]*\r?\n"
            r"^!gov_spending = 0\.0934 \(target g/y = 11\.37%\)[ \t]*\r?\n"
            r"^!Latest version has m(?:_NX)?==0\. It used to be m(?:_NX)? = 0\.1357 \(target \(inv\+nx\)/y = 16\.52%\)\.[ \t]*\r?\n",
            re.MULTILINE,
        ),
    ),
    (
        "P4a: CLUSTER_NOTE 'save results' block",
        re.compile(
            r"^[ \t]*! @@@ CLUSTER_NOTE: I WOULD NORMALLY SAVE THE RESULTS FROM THE CURRENT ITERATION HERE\.[ \t]*\r?\n"
            r"^[ \t]*! NOW, I WILL MOVE IT TO AFTER CONVERGENCE IS ACHIEVED\.[ \t]*\r?\n",
            re.MULTILINE,
        ),
    ),
    (
        "P4b: CLUSTER_NOTE 'indicator_external' line",
        re.compile(
            r"^[ \t]*! @@@ CLUSTER_NOTE: IF WE SET indicator_external to zero, THEN it solves the model from scratch\.[ \t]*\r?\n",
            re.MULTILINE,
        ),
    ),
]


# Pattern 5 is a header REPLACEMENT (not a deletion). Applied only to the one
# file in the package whose header still says "Sovereign Defaults and Banking
# Crises" instead of the corrected "Optimal Bailouts in Banking and Sovereign
# Crises".

P5_FILES = ["Tables-section_C2/bailouts_optimal_mu05_nobailout.f90"]
P5_OLD = (
    "! Code for \"Sovereign Defaults and Banking Crises\"\n"
    "! Adapated to avoid using ISML. It's ready to be used w/ gfortran.\n"
)
P5_NEW = (
    "! Code for \"Optimal Bailouts in Banking and Sovereign Crises\" (Hur, Sosa-Padilla and Yom)\n"
    "!\n"
)


def process_file(path: Path, apply: bool, backup: bool) -> dict[str, int]:
    """Run every pattern on `path`. Return a dict {pattern_label: hits}."""
    original = path.read_text()
    new_text = original
    hits = {}

    for label, pat in PATTERNS:
        new_text, n = pat.subn("", new_text)
        if n:
            hits[label] = n

    # Pattern 5 — only on the one tagged file.
    if any(str(path).endswith(f) for f in P5_FILES):
        if P5_OLD in new_text:
            new_text = new_text.replace(P5_OLD, P5_NEW, 1)
            hits["P5: replace old C.2 header"] = 1

    if new_text != original:
        if apply:
            if backup:
                shutil.copy2(path, path.with_suffix(path.suffix + ".bak"))
            path.write_text(new_text)

    return hits


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("root", type=Path, help="Replication directory to clean (recursive).")
    ap.add_argument("--apply", action="store_true", help="Actually write changes. Default is dry-run.")
    ap.add_argument("--backup", action="store_true", help="Save a .bak copy of every modified file (only with --apply).")
    args = ap.parse_args()

    if not args.root.is_dir():
        print(f"error: {args.root} is not a directory", file=sys.stderr)
        return 1

    files = sorted(args.root.rglob("*.f90"))
    if not files:
        print(f"warning: no .f90 files found under {args.root}", file=sys.stderr)
        return 1

    print(f"{'APPLY' if args.apply else 'DRY-RUN'}: scanning {len(files)} .f90 files under {args.root}")
    print()

    totals: dict[str, int] = {}
    files_changed = 0

    for path in files:
        hits = process_file(path, apply=args.apply, backup=args.backup)
        if hits:
            files_changed += 1
            rel = path.relative_to(args.root)
            summary = ", ".join(f"{label.split(':')[0]}×{n}" for label, n in hits.items())
            print(f"  {rel}  [{summary}]")
            for label, n in hits.items():
                totals[label] = totals.get(label, 0) + n

    print()
    print(f"Files changed: {files_changed} / {len(files)}")
    print("Removals by pattern:")
    for label, n in sorted(totals.items()):
        print(f"  {label}: {n}")

    if not args.apply:
        print()
        print("(dry-run — nothing written. Re-run with --apply to commit.)")

    return 0


if __name__ == "__main__":
    sys.exit(main())
