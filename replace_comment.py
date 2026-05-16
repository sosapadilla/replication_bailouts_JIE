from pathlib import Path

old = """! Code for "Sovereign Defaults and Banking Crises" (Sosa-Padilla, 2015 version)
! Adapated to avoid using ISML. It's ready to be used w/ gfortran.
"""

new = """! Code for "Optimal Bailouts in Banking and Sovereign Crises" (Hur, Sosa-Padilla and Yom)
!
"""

count = 0

for path in Path(".").rglob("*.f90"):
    data = path.read_bytes()

    # Preserve each file's line endings
    if b"\r\n" in data:
        newline = b"\r\n"
    else:
        newline = b"\n"

    old_bytes = old.replace("\n", newline.decode("ascii")).encode("utf-8")
    new_bytes = new.replace("\n", newline.decode("ascii")).encode("utf-8")

    if old_bytes in data:
        occurrences = data.count(old_bytes)
        path.write_bytes(data.replace(old_bytes, new_bytes))
        count += occurrences
        print(f"Replaced {occurrences} occurrence(s) in {path}")

print(f"Done. Replaced {count} total occurrence(s).")