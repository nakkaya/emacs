"""emacs build file."""

import sys
import os
from datetime import datetime
import platform
from invoke import task

def tag(n):
    """Create tag command."""
    t_str = datetime.now().strftime("%Y_%m_%d_%H_%M_%S")
    return ("--tag nakkaya/" + n + ":latest " +
            "--tag ghcr.io/nakkaya/" + n + ":latest " +
            "--tag nakkaya/" + n + ":" + t_str + " ")


@task
def build(c, march=False):
    """Build Multi Arch CPU Image."""
    os.chdir("devops/docker/")

    def cmd(builder):
        return "docker " + builder + " -f Dockerfile " + tag("emacs") + " "

    if march:
        c.run("docker build -f Dockerfile " + tag("emacs") + " .")
    else:
        # c.run("docker buildx build --push -f Dockerfile " + tag("emacs") +
        #       " --platform linux/amd64 .")
        c.run("docker buildx build --push -f Dockerfile " + tag("emacs") +
              " --platform linux/amd64,linux/arm64 .")


@task(auto_shortflags=False,
      help={'with-host': 'Enable host networking.',
            'with-passwd': 'Set login password.',
            'with-gpu': 'Enable GPU in container.',
            'with-docker': 'Mounts host docker socket into container.',
            'with-syncthing': 'Enable Syncthing.',
            'with-jupyter': 'Enable Jupyter.',
            'with-pgadmin': 'Enable pgAdmin.',
            'restart': 'Stop/Remove/Start running container.'})
def docker(c,
           with_host=False,
           with_passwd=None,
           with_gpu=False,
           with_docker=False,
           with_syncthing=False,
           with_jupyter=False,
           with_pgadmin=False,
           restart=False):
    """Launch emacsd Docker Image."""
    c.run("docker pull nakkaya/emacs:latest")

    if restart:
        try:
            c.run("docker stop emacsd")
        except Exception:
            pass

        try:
            c.run("docker container rm emacsd")
        except Exception:
            pass

    if with_host:
        host = "--network host"
    else:
        host = """
        -p 2222:2222/tcp
        -p 4242:4242/tcp
        -p 9090:9090/tcp
        """

    passwd = "--env PASSWD=" + with_passwd if with_passwd else ""
    gpu = "--gpus all" if with_gpu else ""

    docker_sock = ""
    if with_docker:
        import grp
        group_info = grp.getgrnam('docker')
        group_id = group_info[2]

        docker_sock = \
            "--group-add " + str(group_id) + " " + \
            "-v /var/run/docker.sock:/var/run/docker.sock"

    syncthing = "--env SYNCTHING_ENB=1" if with_syncthing else ""

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

    volumes = [["emacsd-sshd", "/etc/ssh"],
               ["emacsd-home", "/home/core"],
               ["emacsd-storage", "/storage"]]

    volume_mounts = ""

    for v in volumes:
        v_host, v_docker = v
        volume_mounts = volume_mounts + " -v " + v_host + ":" + v_docker + " "

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


    filename = "./emacsd"
    with open(filename, 'w') as f:
        inv_cmd = ' '.join(sys.argv)
        f.write(f'#!/bin/bash\n\nsudo {inv_cmd}\n')
    os.chmod(filename, 0o755)

    c.run(cmd)
