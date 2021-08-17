import re as _re
import os as _os
from glob import glob as _glob
import fileinput as _fileinput
from pathlib import Path as _Path

_python_defun = _re.compile(r'^\s*def\s.+[^\\(]\n$')


def fetch_from(dir, output):
    import typing as _T
    pathdir = _Path(dir)
    if not pathdir.is_dir():
        raise FileNotFoundError(str(pathdir))
    cwdsave = _os.getcwd()
    _os.chdir(dir)
    lines: _T.List[str] = list()
    for line in _fileinput.input(_glob('**/*.py', recursive=True)):
        line = str(line)
        if _python_defun.match(line) and not line.endswith(r'('):
            lines.append(line)
    _os.chdir(cwdsave)
    pathout = _Path.cwd().joinpath("data").joinpath(output)
    with open(pathout, 'w') as hnd:
        hnd.writelines(lines)
