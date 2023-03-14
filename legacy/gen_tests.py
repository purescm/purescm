#!/usr/bin/env python

import os
import re
import subprocess
import shutil
from pathlib import Path


PURESCRIPT_PATH = Path("test/resources/purescript")
SOURCE_PATH = PURESCRIPT_PATH / "src"
OUTPUT_PATH = PURESCRIPT_PATH / "output"
SCHEME_PATH = Path("test/resources/scheme")
MODULE_REGEX = re.compile("\s*module\s+([\w\.]+)", re.MULTILINE)


def run(cmd: list[str], path: str, env=None):
    old_dir = os.getcwd()
    os.chdir(path)
    subprocess.run(cmd, env=env)
    os.chdir(old_dir)


def get_source_files(base_path: Path) -> list[Path]:
    return Path(base_path).glob("**/*.purs")


def read_module_name(path: Path) -> str:
    text = path.read_text()
    match = MODULE_REGEX.match(text)
    return match.group(1)


def copy_module(output_path: Path, scheme_path: Path, module_name: str):
    from_file = output_path / module_name / "lib.sls"
    to_dir = scheme_path / module_name
    to_file = to_dir / "lib.sls"
    os.makedirs(to_dir, exist_ok=True)
    print(f"{from_file} -> {to_file}")
    shutil.copy2(from_file, to_file)


if __name__ == "__main__":
    run(["stack", "install", "--fast", "--local-bin-path", "/tmp"],
        PURESCRIPT_PATH)
    run(["spago", "build"],
        PURESCRIPT_PATH,
        {'PATH': "/tmp:" + os.environ["PATH"]})

    files = get_source_files(SOURCE_PATH)
    modules = [read_module_name(f) for f in files]

    for module in modules:
        copy_module(OUTPUT_PATH, SCHEME_PATH, module)
