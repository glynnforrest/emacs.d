#!/usr/bin/env python3

import json
import subprocess
import os
import sys

def usage():
    print("""
    Usage: {0} [COMMAND]

    Manage a collection of git repositories.

    Available commands:

    {0} check
    {0} clone
""".format(os.path.basename(sys.argv[0])))

def problem(message):
    raise Exception("{}".format(message))

def git_clone(url, target):
    target = os.path.expanduser(target)
    parent_dir = os.path.dirname(target)
    if not os.path.isdir(parent_dir):
        print("Creating directory {}".format(parent_dir))

    if os.path.isdir(target.rstrip("/")+"/.git"):
        return

    print("Cloning {} to {}".format(url, target))
    command = "git clone {} {}".format(url, target)
    process = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
    for c in iter(lambda: process.stdout.read(1), b''):
        sys.stdout.write(c)

def clone_repos(file):
    failures = []
    with open(file) as json_data:
        for repo in json.load(json_data):
            git_clone(repo['url'], repo['target'])

def git_check(command, dir):
    return subprocess.check_output(command, shell=True, cwd=dir).strip().splitlines()

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
                dir = os.path.expanduser(repo[u'target'])
                check_repo(dir)
            except Exception as e:
                failures.append(repo[u'target'])
                print(e)

    if len(failures) > 0:
        print("")
        print("{} {} unpushed changes.".format(len(failures), 'repo has' if len(failures) == 1 else 'repos have'))
        exit(1)

    print("All repos are synced with the remotes.")

if len(sys.argv) < 2:
    usage()
    exit(1)

cmd = sys.argv[1]

if cmd == "clone":
    clone_repos(os.path.expanduser('~/.repos.json'))
elif cmd == "check":
    check_repos(os.path.expanduser('~/.repos.json'))
else:
    usage()
    exit(1)
