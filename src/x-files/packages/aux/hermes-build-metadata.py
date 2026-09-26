"""Use Guix's dependency closure instead of PyPI version pins."""

from pathlib import Path

from packaging.requirements import Requirement
from packaging.specifiers import SpecifierSet
import tomlkit


def main():
    path = Path("pyproject.toml")
    document = tomlkit.parse(path.read_text())
    requirements = []
    for value in document["project"]["dependencies"]:
        requirement = Requirement(value)
        # Both have supported runtime fallbacks.  Neither is used by the
        # Codex worker's file tools; they are not advertised as installed.
        if requirement.name in {"nemo-relay", "firecrawl-anydoc"}:
            continue
        requirement.specifier = SpecifierSet()
        requirements.append(str(requirement))
    document["project"]["dependencies"] = requirements
    path.write_text(tomlkit.dumps(document))


if __name__ == "__main__":
    main()
