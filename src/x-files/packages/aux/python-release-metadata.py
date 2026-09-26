"""Use a Python release archive's resolved metadata without Git or uv."""

from email.parser import Parser
from pathlib import Path

import tomlkit


def main():
    """Replace dynamic version and dependencies with the sdist metadata."""
    path = Path("pyproject.toml")
    document = tomlkit.parse(path.read_text())
    metadata = Parser().parsestr(Path("PKG-INFO").read_text())
    project = document["project"]
    project["version"] = metadata["Version"]
    project["dependencies"] = [
        requirement
        for requirement in metadata.get_all("Requires-Dist", [])
        if "extra ==" not in requirement
    ]
    project["dynamic"] = [
        field
        for field in project.get("dynamic", [])
        if field not in ("version", "dependencies")
    ]
    document["build-system"]["requires"] = [
        requirement
        for requirement in document["build-system"]["requires"]
        if not requirement.startswith("uv-dynamic-versioning")
    ]
    hatch = document.get("tool", {}).get("hatch", {})
    hatch.pop("version", None)
    hatch.get("metadata", {}).get("hooks", {}).pop("uv-dynamic-versioning", None)
    path.write_text(tomlkit.dumps(document))


if __name__ == "__main__":
    main()
