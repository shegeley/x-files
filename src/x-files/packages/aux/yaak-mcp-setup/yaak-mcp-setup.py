#!/usr/bin/env python3
"""
Ensure a plugin entry exists in Yaak's SQLite database, pointing at a given
plugin directory. Safe to run multiple times (idempotent).

Usage: yaak-mcp-setup.py <plugin-dir> <db-path>
"""

import sys
import sqlite3
import random
import string
from datetime import datetime, timezone

def main():
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} <plugin-dir> <db-path>", file=sys.stderr)
        sys.exit(1)

    plugin_dir = sys.argv[1]
    db_path = sys.argv[2]

    conn = sqlite3.connect(db_path)
    cur = conn.cursor()

    cur.execute("SELECT id FROM plugins WHERE directory = ?", (plugin_dir,))
    if cur.fetchone() is not None:
        print("yaak-mcp-setup: plugin already registered, nothing to do")
        conn.close()
        return

    now = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M:%S.%f")
    pid = "pg_" + "".join(random.choices(string.ascii_letters + string.digits, k=10))
    cur.execute(
        "INSERT INTO plugins (id, model, created_at, updated_at, checked_at, enabled, directory, url)"
        " VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
        (pid, "plugin", now, now, None, 1, plugin_dir, None),
    )
    conn.commit()
    conn.close()
    print(f"yaak-mcp-setup: inserted plugin entry {pid}")

if __name__ == "__main__":
    main()
