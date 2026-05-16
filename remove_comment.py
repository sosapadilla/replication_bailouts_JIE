from pathlib import Path

target_lf = """!ivalue=139719 -- This is what I had in the JMP
"""

count = 0

for path in Path(".").rglob("*.f90"):
    data = path.read_bytes()

    # Detect the line ending style of this particular file
    if b"\r\n" in data:
        newline = b"\r\n"
    else:
        newline = b"\n"

    target = target_lf.replace("\n", newline.decode("ascii")).encode("utf-8")

    if target in data:
        occurrences = data.count(target)
        new_data = data.replace(target, b"")
        path.write_bytes(new_data)

        count += occurrences
        print(f"Removed {occurrences} occurrence(s) from {path}")

print(f"Done. Removed {count} total occurrence(s).")