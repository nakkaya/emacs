"""emacsd build file."""

from invoke import task
import subprocess
import os
import sys
sys.tracebacklimit = 0

version = subprocess.check_output(["git", "describe", "--always"])
version = version.strip().decode('UTF-8')


def tag(n):
    """Create tag command."""
    return "--tag nakkaya/" + n + ":latest " +
           "--tag ghcr.io/nakkaya/" + n + ":latest " +
           "--tag nakkaya/" + n + ":" + version + " "


def run(cmd, dir="."):
    """Run cmd in dir."""
    wd = os.getcwd()
    os.chdir(dir)
    subprocess.check_call(cmd, shell=True)
    os.chdir(wd)


@task
def build(ctx):
    """Build Images."""
    cmd = "docker build "
#    cmd = "docker build --no-cache "

    run(cmd + "-f Dockerfile.emacs " + tag("emacs") + " .", "devops/docker/")
    run(cmd + "-f Dockerfile.gpu " + tag("gpu") + " .", "devops/docker/")

@task
def buildx(ctx):
    """Build Multi Arch Images."""
    cmd = "docker buildx build --push "

    run(cmd +
        "-f Dockerfile.emacs " + tag("emacs") +
        " --platform linux/amd64 .",
        "devops/docker/")

    run(cmd +
        "-f Dockerfile.gpu " + tag("gpu") +
        " --platform linux/amd64 .",
        "devops/docker/")

@task
def push(ctx):
    """Push Images to DockerHub."""
    run("docker push nakkaya/emacs:latest")
    run("docker push nakkaya/emacs:" + version)
    run("docker push nakkaya/gpu:latest")
    run("docker push nakkaya/gpu:" + version)

@task
def push_ghcr(ctx):
    """Push Images to GHCR"""
    run("docker push ghcr.io/nakkaya/emacs:latest")
    run("docker push ghcr.io/nakkaya/gpu:latest")

@task
def pull(ctx):
    """Pull emacsd from DockerHub."""
    run("docker pull nakkaya/emacs:latest")


@task
def up(ctx):
    """Start emacsd."""
    run("docker-compose up -d", "devops/docker/")


@task
def down(ctx):
    """Stop emacsd."""
    run("docker-compose down", "devops/docker/")


@task
def gui(ctx):
    """Start emacsd with X-Forwarding."""
    files = "-f docker-compose.yml -f docker-compose.gui.linux.yml"
    run("docker-compose " + files + " up -d", "devops/docker/")
