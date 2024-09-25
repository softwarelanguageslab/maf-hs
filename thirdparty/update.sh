#!/usr/bin/env python3

"""
Script to update the vendored libraries based on the `dependencies.txt` 
file.
"""

import os 
from urllib.parse import urlparse
from pathlib import Path
import subprocess

def ungit(dirname): 
    os.rename(dirname+"/.git", dirname+"/.git.vendored")

def regit(dirname):
    os.rename(dirname+"/.git.vendored", dirname+"/.git")

def update_dependency(dirname, commit_hash):
    regit(dirname)
    os.chdir(dirname)
    subprocess.run(["git", "fetch", "origin"])
    subprocess.run(["git", "checkout", commit_hash])
    os.chdir("..")
    ungit(dirname)


def clone_dependency(url, dirname):
    subprocess.run(["git", "clone", url, dirname])
    ungit(dirname)

# change the working directory to the directory of the script
dir_path = os.path.dirname(os.path.realpath(__file__))
os.chdir(dir_path)

# then look at the dependencies
with open("dependencies.txt") as f: 
   deps = [ line.split() for line in f.readlines() ]
   
   # Clone/pull dependencies if necessary
   for [url_text, commit_hash] in deps: 
        url     = urlparse(url_text)
        path    = Path(url.path)
        dirname = path.name
        if path.exists(): 
            update_dependency(dirname, commit_hash)
        else: 
            clone_dependency(url_text, dirname) 
            update_dependency(dirname, commit_hash)

        print("Dependency " + dirname + " updated. ")
