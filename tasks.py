"""emacs build file."""

from invoke import task
import subprocess
import os
import glob

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


def docker(builder, type, *arg):
    """Run docker command."""
    cmd = ("docker " + builder +
           " -f Dockerfile " + tag("emacs-" + type) +
           " ".join(arg) + " .")
    run(cmd, "devops/docker/")


gpu_image = 'BASE_IMAGE=ghcr.io/nakkaya/emacsd-gpu'


@task
def build_cpu(ctx):
    """Build CPU Image."""
    docker("build", "cpu")


@task
def build_gpu(ctx):
    """Build GPU Image."""
    docker("build", "gpu", "--build-arg", gpu_image, "--build-arg IMAGE_TYPE=GPU")
    run("docker push ghcr.io/nakkaya/emacs-gpu:latest")
    run("docker push --all-tags nakkaya/emacs-gpu")


@task
def buildx_cpu(ctx):
    """Build Multi Arch CPU Image."""
    docker("buildx build --push", "cpu",
           "--platform linux/amd64,linux/arm64"
           " --build-arg IMAGE_TYPE=CPU")


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
