"""emacs build file."""

from invoke import task
import subprocess
import os
from datetime import datetime
import platform
from os.path import expanduser
from pathlib import Path


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


def docker_build(builder, *arg):
    """Run docker command."""
    cmd = ("docker " + builder +
           " -f Dockerfile " +
           tag("emacs") +
           " ".join(arg) + " .")
    run(cmd, "devops/docker/")


@task
def build(ctx):
    """Build Multi Arch CPU Image."""
    docker_build("buildx build --push", "--platform linux/amd64,linux/arm64")


@task
def docker(ctx,
           with_host=False,
           with_passwd=None,
           with_gpu=False,
           with_docker=False,
           restart=False):
    """Launch Docker Image."""

    if restart:
        run("docker stop emacsd")
        run("docker container rm emacsd")

    if with_host:
        host = "--network host"
    else:
        host = """
        -p 2222:2222/tcp
        -p 4242:4242/tcp
        -p 9090:9090/tcp
        """

    passwd = ""
    if with_passwd:
        passwd = "--env PASSWD=" + with_passwd

    gpu = ""
    if with_gpu:
        gpu = "--gpus all"

    docker_sock = ""
    if with_docker:
        import grp
        group_info = grp.getgrnam('docker')
        group_id = group_info[2]

        docker_sock = \
            "--group-add " + str(group_id) + " " + \
            "-v /var/run/docker.sock:/var/run/docker.sock"

    run("docker volume create emacsd-home")
    run("docker volume create emacsd-storage")
    run("docker volume create emacsd-sandbox")
    run("docker volume create emacsd-dataset")
    run("docker volume create emacsd-pgadmin")

    volumes = [["emacsd-home", "/home/core"],
               ["emacsd-storage", "/storage"],
               ["emacsd-sandbox", "/sandbox"],
               ["emacsd-dataset", "/dataset"],
               ["emacsd-pgadmin", "/var/lib/pgadmin"], ]

    volume_mounts = ""

    for v in volumes:
        v_host, v_docker = v
        volume_mounts = volume_mounts + " -v " + v_host + ":" + v_docker + " "

    run("docker pull nakkaya/emacs:latest")

    cmd = """
    docker run
    --privileged
    --security-opt=\"seccomp=unconfined\"
    --restart=always
    --name emacsd
    --detach
    """ + host + """
    --hostname """ + platform.node() + """
    """ + passwd + """
    """ + volume_mounts + """
    """ + docker_sock + """
    """ + gpu + """
    nakkaya/emacs"""

    cmd = cmd.replace('\n', ' ')
    cmd = ' '.join(cmd.split())
    print(cmd)
    run(cmd)
