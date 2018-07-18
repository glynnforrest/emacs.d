#!/usr/bin/env python3

import json
import subprocess
import os
import sys
from operator import itemgetter

def usage():
    print("""
    Usage: {0} [COMMAND]

    Manage a collection of git repositories.

    Available commands:

    {0} check
    {0} clone
    {0} add [URL] [TARGET]
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

def add_repo(file, url, target):
    home = os.path.expanduser("~")
    if home in target:
        target = "~/" + os.path.relpath(os.path.expanduser(target), home)

    repos = []
    try:
        with open(file) as json_data:
            repos = json.load(json_data)
            for repo in repos:
                if repo['target'] == target:
                    print("Target {} already exists in {}.".format(target, file))
                    return
            repos.append({
                'url': url,
                'target': target
            })
            repos = sorted(repos, key=itemgetter('target'))
    except Exception as e:
        print(e)
        pass

    with open(file, mode='w+') as save_file:
        json.dump(repos, save_file, indent=4)

if len(sys.argv) < 2:
    usage()
    exit(1)

cmd = sys.argv[1]
config_file = os.path.expanduser('~/.repos.json')

if cmd == "clone":
    clone_repos(config_file)
elif cmd == "check":
    check_repos(config_file)
elif cmd == "add" and len(sys.argv) == 4:
    add_repo(config_file, sys.argv[2], sys.argv[3])
else:
    usage()
    exit(1)
