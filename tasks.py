from invoke import task
import subprocess
import os
import time
import sys
sys.tracebacklimit = 0

version = subprocess.check_output(["git", "describe", "--always"]).strip().decode('UTF-8')

def tag(n):
    return "-t nakkaya/" + n + ":latest -t nakkaya/" + n + ":" + version

def run(cmd, dir = "."):
    wd = os.getcwd()
    os.chdir(dir)
    subprocess.check_call(cmd, shell=True)
    os.chdir(wd)

@task
def build(ctx):
    run("docker build -f Dockerfile.env -t nakkaya/env:latest .", "devops/docker/")
    run("docker build -f Dockerfile.emacs " + tag("emacs") + " .", "devops/docker/")
    #run("docker build --no-cache -f Dockerfile.emacs " + tag("emacs") + " ." , "devops/docker/")

@task
def push(ctx):
    run("docker push nakkaya/emacs:latest")
    run("docker push nakkaya/emacs:" + version)

@task
def pull(ctx):
    run("docker pull nakkaya/emacs:latest")

@task
def up(ctx):
    run("docker-compose up -d", "devops/docker/")

@task
def down(ctx):
    run("docker-compose down", "devops/docker/")
