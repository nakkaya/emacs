"""emacs build file."""

import os
from datetime import datetime
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
