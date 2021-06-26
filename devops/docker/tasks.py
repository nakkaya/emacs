from invoke import task
import subprocess
import os
import time
import sys
sys.tracebacklimit = 0

version = subprocess.check_output(["git", "describe", "--always"]).strip().decode('UTF-8')

def tag(n):
    return "-t nakkaya/" + n + ":latest -t nakkaya/" + n + ":" + version

def run(cmd):
    subprocess.check_call(cmd, shell=True)

@task
def build(ctx):
    run("docker build -f Dockerfile.env -t nakkaya/env:latest .")
    run("docker build -f Dockerfile.emacs " + tag("emacs") + " .")
    #run("docker build --no-cache -f Dockerfile.emacs " + tag("emacs") + " .")

@task
def push(ctx):
    run("docker push nakkaya/emacs:latest")
    run("docker push nakkaya/emacs:" + version)

@task
def pull(ctx):
    run("docker pull nakkaya/emacs:latest")

@task
def up(ctx):
    subprocess.check_call("docker-compose up -d", shell=True)

@task
def down(ctx):
    subprocess.check_call("docker-compose down", shell=True)
