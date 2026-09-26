"""Validate Macher's generated tool schemas offline against JSON Schema 2020-12."""

import json
import sys

from jsonschema import Draft202012Validator


if __name__ == "__main__":
    # Match the test's `jsonschema validate META-SCHEMA INPUT-SCHEMA` arguments.
    if len(sys.argv) != 4 or sys.argv[1] != "validate":
        raise SystemExit("usage: macher-validate-schema.py validate META INPUT")
    with open(sys.argv[3], encoding="utf-8") as source:
        Draft202012Validator.check_schema(json.load(source))
