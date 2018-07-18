#!/usr/bin/python

# check if git repositories have any local changes that haven't been pushed.
# checks for local branches, stashes, and changed or untracked files.

# todo: report different violations for all repos in a table

import json
from pprint import pprint
from subprocess import check_output
import os


check_repos(os.path.expanduser('~/.repos.json'))
