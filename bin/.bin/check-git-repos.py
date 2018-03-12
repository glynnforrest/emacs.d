#!/usr/bin/python

# check if git repositories have any local changes that haven't been pushed.
# checks for local branches, stashes, and changed or untracked files.

# todo: report different violations for all repos in a table

import json
from pprint import pprint
from subprocess import check_output
import os

def problem(message):
    raise Exception("{}".format(message))

def git_check(command, dir):
    return check_output(command, shell=True, cwd=dir).strip().splitlines()

def check_repo(dir):
    if not os.path.isdir(dir):
        problem("{} does not exist.".format(dir))

    unpushed_commits = git_check('git log --branches --not --remotes --oneline', dir)
    if len(unpushed_commits) > 0:
        problem("{} has {} unpushed {}.".format(dir, len(unpushed_commits), 'commit' if len(unpushed_commits) == 1 else 'commits'))

    stashes = git_check('git stash list', dir)
    if len(stashes) > 0:
        problem("{} has {} {}.".format(dir, len(stashes), 'stash' if len(stashes) == 1 else 'stashes'))

    changed_files = git_check('git status --short', dir)
    if len(changed_files) > 0:
        problem("{} has {} changed or untracked {}.".format(dir, len(changed_files), 'file' if len(changed_files) == 1 else 'files'))


def check_repos(file):
    failures = []
    with open(file) as json_data:
        for repo in json.load(json_data):
            try:
                dir = repo[u'target']
                check_repo(dir)
            except Exception as e:
                failures.append(repo[u'target'])
                print("!! "+e.message)

    if len(failures) > 0:
        print ""
        print "{} {} unpushed changes.".format(len(failures), 'repo has' if len(failures) == 1 else 'repos have')
        exit(1)

    print "All repos are synced with the remotes."

check_repos(os.path.expanduser('~/.repos.json'))
