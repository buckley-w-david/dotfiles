#!/usr/bin/env python
import pathlib
import sys
import os

raw_path = sys.argv[1].replace(str(pathlib.Path.home()), '~')
path = pathlib.Path(raw_path)
parts = [i[0] for i in path.parts[:-1]] + [path.parts[-1]]
print(os.path.join(*parts))
