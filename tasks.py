"""emacs build file."""

from invoke import task
import subprocess
import os
from datetime import datetime
import platform
from os.path import expanduser
from pathlib import Path
import grp


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
def docker(ctx, with_host=False, with_passwd=None, with_gpu=False, with_docker=False):
    """Launch Docker Image."""
    if with_host:
        host = "--network host"
    else:
        host = """
        -p 2222:2222/tcp
        -p 4242:4242/tcp
        -p 9090:9090/tcp
        """

    if with_passwd:
        passwd = "--env PASSWD=" + with_passwd
    else:
        passwd = ""

    if with_gpu:
        gpu = "--gpus all"
    else:
        gpu = ""

    if with_docker:
        group_info = grp.getgrnam('docker')
        group_id = group_info[2]

        docker_sock = \
            "--group-add " + str(group_id) + " " + \
            "-v /var/run/docker.sock:/var/run/docker.sock"
    else:
        docker_sock = ""

    home = expanduser("~") + "/.emacsd"
    volumes = [["/storage", "/storage"],
               ["/.emacs.d", "/home/core/.emacs.d"],
               ["/sandbox", "/sandbox"],
               ["/dataset", "/dataset"],
               ["/pgadmin", "/var/lib/pgadmin"],
               ["/gcloud", "/home/core/.config/gcloud"],
               ["/syncthing", "/home/core/.config/syncthing"],
               ["/jupyter", "/home/core/.local/share/jupyter"],
               ["/conda", "/home/core/.conda"],
               ["/skypilot", "/home/core/.sky"],
               ["/lein", "/home/core/.lein"],
               ["/clojure", "/home/core/.clojure"],
               ["/deps", "/home/core/.deps.clj"],
               ["/mvn", "/home/core/.m2"],
               ["/qutebrowser/data", "/home/core/.local/share/qutebrowser"],
               ["/qutebrowser/cache", "/home/core/.cache/qutebrowser"], ]

    volume_mounts = ""

    for v in volumes:
        v_host, v_docker = v
        v_host = home + v_host
        Path(v_host).mkdir(parents=True, exist_ok=True)
        volume_mounts = volume_mounts + " -v " + v_host + ":" + v_docker + " "

    run("docker pull nakkaya/emacs:latest")
    run("docker stop emacsd")
    run("docker container rm emacsd")
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
