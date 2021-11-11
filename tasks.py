"""emacsd build file."""

from invoke import task
import subprocess
import os
import sys
import glob
sys.tracebacklimit = 0

version = subprocess.check_output(["git", "describe", "--always"])
version = version.strip().decode('UTF-8')


def tag(n):
    """Create tag command."""
    return ("--tag nakkaya/" + n + ":latest " +
            "--tag ghcr.io/nakkaya/" + n + ":latest " +
            "--tag nakkaya/" + n + ":" + version + " ")


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

    run(cmd + "-f Dockerfile.emacs " + tag("emacs-cpu") + " .", "devops/docker/")

gpu_image = 'BASE_IMAGE=ghcr.io/nakkaya/emacsd-gpu'

@task
def buildx(ctx):
    """Build Multi Arch Images."""
    cmd = "docker buildx build --push "

    run(cmd +
        " -f Dockerfile.emacs " + tag("emacs-gpu") +
        " --platform linux/amd64 " +
        " --build-arg " + gpu_image  + " .",
        "devops/docker/")

    run(cmd +
        "-f Dockerfile.emacs " + tag("emacs-cpu") +
        " --platform linux/amd64 .",
        "devops/docker/")

    run(cmd +
        "-f Dockerfile.gpu " + tag("gpu") +
        " --platform linux/amd64 .",
        "devops/docker/")

def compose_files():
    files = glob.glob('devops/docker/docker-compose*.yml')
    files = [" -f " + os.path.basename(f) for f in files]
    files =  ' '.join(files)
    return files

@task
def up(ctx, with_gpu=False):
    """Start emacsd."""
    compose_files = " -f docker-compose.emacsd.yml"
    if with_gpu :
        compose_files = compose_files + " -f docker-compose.with.gpu.yml"
    run("docker-compose " + compose_files + " up -d", "devops/docker/")

@task
def down(ctx):
    """Stop emacsd."""
    run("docker-compose " + compose_files() + " down", "devops/docker/")
