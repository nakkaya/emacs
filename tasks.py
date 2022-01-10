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

    run(cmd + "-f Dockerfile " + tag("emacs-cpu") + " .",
        "devops/docker/")


gpu_image = 'BASE_IMAGE=ghcr.io/nakkaya/emacsd-gpu'
buildx_cmd = "docker buildx build --push "


@task
def buildx_gpu(ctx):
    """Build adm64 Image."""
    run(buildx_cmd +
        " -f Dockerfile " + tag("emacs-gpu") +
        " --platform linux/amd64 " +
        " --build-arg " + gpu_image + " .",
        "devops/docker/")


@task
def buildx_cpu(ctx):
    """Build amd64 CPU Image."""
    run(buildx_cmd +
        "-f Dockerfile " + tag("emacs-cpu") +
        " --platform linux/amd64,linux/arm64 .",
        "devops/docker/")

def compose_files():
    """Build list of compose files."""
    files = glob.glob('devops/docker/docker-compose*.yml')
    files = [" -f " + os.path.basename(f) for f in files]
    files = ' '.join(files)
    return files


@task
def up(ctx, with_gpu=False):
    """Start emacsd."""
    compose_files = " -f docker-compose.emacs-cpu.yml"
    if with_gpu:
        compose_files = " -f docker-compose.emacs-gpu.yml"
    run("docker-compose " + compose_files + " up -d", "devops/docker/")


@task
def down(ctx):
    """Stop emacsd."""
    run("docker-compose " + compose_files() + " down", "devops/docker/")
