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

    {0} add [URL] [DIRECTORY]
    {0} check
    {0} check [DIRECTORY]
    {0} clone
    {0} list
""".format(os.path.basename(sys.argv[0])))


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
    problems = []
    if not os.path.isdir(dir):
        problems.append("does not exist")

    unpushed_commits = git_check('git log --branches --not --remotes --oneline', dir)
    if len(unpushed_commits) > 0:
        problems.append("{} unpushed {}".format(len(unpushed_commits), 'commit' if len(unpushed_commits) == 1 else 'commits'))

    stashes = git_check('git stash list', dir)
    if len(stashes) > 0:
        problems.append("{} {}".format(len(stashes), 'stash' if len(stashes) == 1 else 'stashes'))

    changed_files = git_check('git status --short', dir)
    if len(changed_files) > 0:
        problems.append("{} changed or untracked {}".format(len(changed_files), 'file' if len(changed_files) == 1 else 'files'))

    if len(problems) > 0:
        raise Exception("{}".format(", ".join(problems)))

def check_repos(file):
    with open(file) as json_data:
        repos = json.load(json_data)

        # do a quick loop without git commands first to get the max
        # directory length for print formatting
        max_dir_length = 0
        for repo in repos:
            dir_length = len(repo[u'target'])
            if dir_length > max_dir_length:
                max_dir_length = dir_length

        failures = []
        for repo in repos:
            try:
                dir = repo[u'target']
                check_repo(os.path.expanduser(dir))
            except Exception as e:
                failures.append(dir)
                print(dir.ljust(max_dir_length) + "  " + str(e))

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
        save_file.write('\n')

def list_dirs(file):
    with open(file) as json_data:
        for repo in json.load(json_data):
            print(os.path.expanduser(repo['target'].strip()))

if len(sys.argv) < 2:
    usage()
    exit(1)

cmd = sys.argv[1]
config_file = os.path.expanduser('~/.repos.json')

if cmd == "clone":
    clone_repos(config_file)
elif cmd == "check":
    if len(sys.argv) == 3:
        try:
            check_repo(os.path.expanduser(sys.argv[2]))
        except Exception as e:
            print(e)
            exit(1);
    else:
        check_repos(config_file)
elif cmd == "add" and len(sys.argv) == 4:
    add_repo(config_file, sys.argv[2], sys.argv[3])
elif cmd == "list":
    list_dirs(config_file)
else:
    usage()
    exit(1)
