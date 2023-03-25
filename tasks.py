"""emacs build file."""

from invoke import task
import subprocess
import os
from datetime import datetime
import platform


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
def build(ctx, march=False):
    """Build Multi Arch CPU Image."""
    if march:
        docker_build("build")
    else:
        docker_build("buildx build --push", "--platform linux/amd64,linux/arm64") # noqa


@task(auto_shortflags=False,
      help={'with-host': 'Enable host networking.',
            'with-passwd': 'Set login password.',
            'with-gpu': 'Enable GPU in container.',
            'with-docker': 'Mounts host docker socket into container.',
            'with-syncthing': 'Enable Syncthing.',
            'with-jupyter': 'Enable Jupyter.',
            'with-pgadmin': 'Enable pgAdmin.',
            'restart': 'Stop/Remove/Start running container.'})
def docker(ctx,
           with_host=False,
           with_passwd=None,
           with_gpu=False,
           with_docker=False,
           with_syncthing=False,
           with_jupyter=False,
           with_pgadmin=False,
           restart=False):
    """Launch emacsd Docker Image."""
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

    syncthing = ""
    if with_syncthing:
        syncthing = "--env SYNCTHING_ENB=1"

    jupyter = ""
    if with_jupyter:
        jupyter = "--env JUPYTER_SERVER_ENB=1"
        if not with_host:
            jupyter = jupyter + " -p 8181:8181/tcp"

    pgadmin = ""
    if with_pgadmin:
        pgadmin = "--env PGADMIN_ENB=1"
        if not with_host:
            pgadmin = pgadmin + " -p 5050:5050/tcp"

    run("docker volume create emacsd-home")
    run("docker volume create emacsd-storage")
    run("docker volume create emacsd-sandbox")
    run("docker volume create emacsd-dataset")

    volumes = [["emacsd-home", "/home/core"],
               ["emacsd-storage", "/storage"],
               ["emacsd-sandbox", "/sandbox"],
               ["emacsd-dataset", "/dataset"], ]

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
    """ + syncthing + """
    """ + jupyter + """
    """ + pgadmin + """
    """ + volume_mounts + """
    """ + docker_sock + """
    """ + gpu + """
    nakkaya/emacs"""

    cmd = cmd.replace('\n', ' ')
    cmd = ' '.join(cmd.split())
    print(cmd)
    run(cmd)
