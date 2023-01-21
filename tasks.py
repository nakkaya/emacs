"""emacs build file."""

from invoke import task
import subprocess
import os
import glob
from datetime import datetime


def tag(n):
    """Create tag command."""
    t_str = datetime.now().strftime("%Y_%m_%d_%H_%M_%S")
    return ("--tag nakkaya/" + n + ":latest " +
            "--tag ghcr.io/nakkaya/" + n + ":latest " +
            "--tag nakkaya/" + n + ":" + t_str + " ")


def run(cmd, dir="."):
    """Run cmd in dir."""
    wd = os.getcwd()
    os.chdir(dir)
    subprocess.check_call(cmd, shell=True)
    os.chdir(wd)


def docker(builder, *arg):
    """Run docker command."""
    cmd = ("docker " + builder +
           " -f Dockerfile " +
           tag("emacs") +
           " ".join(arg) + " .")
    run(cmd, "devops/docker/")


@task
def build(ctx):
    """Build Multi Arch CPU Image."""
    docker("buildx build --push", "--platform linux/amd64,linux/arm64")


def compose_files():
    """Build list of compose files."""
    files = glob.glob('devops/docker/docker-compose*.yml')
    files = [" -f " + os.path.basename(f) for f in files]
    files = ' '.join(files)
    return files


@task
def up(ctx, with_gpu=False):
    """Start emacsd."""
    compose_files = " -f docker-compose.yml"
    if with_gpu:
        compose_files = " -f docker-compose.with-gpu.yml"
    run("docker-compose " + compose_files + " up -d", "devops/docker/")


@task
def down(ctx):
    """Stop emacsd."""
    run("docker-compose " + compose_files() + " down", "devops/docker/")
