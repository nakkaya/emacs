#!/bin/bash

if [[ $EUID -ne 0 ]]; then
    echo "$0 is not running as root. Try using sudo."
    exit 2
fi

echo -e "Container Configuration\n"

read -p "Enable host networking (y/n) [n]? " with_host
with_host=${with_host:-n}

while [[ -z "$with_passwd" ]]; do
    read -s -p "Set login password: " with_passwd
    echo
done

read -p "Set container hostname [$(hostname)]? " container_hostname
container_hostname=${container_hostname:-$(hostname)}

read -p "Mount GPU in container (y/n) [n]? " with_gpu
with_gpu=${with_gpu:-n}

read -p "Mount host docker socket into container (y/n) [n]? " with_docker
with_docker=${with_docker:-n}

echo -e "\nService Configuration\n"

read -p "Enable Syncthing (y/n) [n]? " with_syncthing
with_syncthing=${with_syncthing:-n}

read -p "Enable Jupyter (y/n) [n]? " with_jupyter
with_jupyter=${with_jupyter:-n}

read -p "Enable pgAdmin (y/n) [n]? " with_pgadmin
with_pgadmin=${with_pgadmin:-n}

read -p "Enable VSCode (y/n) [n]? " with_vscode
with_vscode=${with_vscode:-n}

if [ "$with_host" == "y" ]; then
    host="--network host"
else
    host="-p 9090:9090/tcp"
fi

passwd="--env PASSWD=$with_passwd"

if [ "$with_gpu" == "y" ]; then
    gpu="--gpus all"
else
    gpu=""
fi

if [ "$with_docker" == "y" ]; then
    group_id=$(getent group docker | cut -d: -f3)
    docker_sock="--group-add $group_id -v /var/run/docker.sock:/var/run/docker.sock"
else
    docker_sock=""
fi

if [ "$with_syncthing" == "y" ]; then
    syncthing="--env SYNCTHING_ENB=1"
else
    syncthing=""
fi

if [ "$with_jupyter" == "y" ]; then
    jupyter="--env JUPYTER_SERVER_ENB=1"
else
    jupyter=""
fi

if [ "$with_pgadmin" == "y" ]; then
    pgadmin="--env PGADMIN_ENB=1"
else
    pgadmin=""
fi

if [ "$with_vscode" == "y" ]; then
    vscode="--env VSCODE_ENB=1"
else
    vscode=""
fi

volume_mounts="-v emacsd-sshd:/etc/ssh -v emacsd-home:/home/core -v emacsd-storage:/storage"

cmd="docker run --privileged \
    --security-opt=\"seccomp=unconfined\" \
    --restart=always \
    --name emacsd \
    --detach \
    $host \
    --hostname $container_hostname \
    $passwd \
    $syncthing \
    $jupyter \
    $pgadmin \
    $vscode \
    $volume_mounts \
    $docker_sock \
    $gpu \
    nakkaya/emacs:latest"

{
    echo "#!/bin/bash"
    echo ""
    echo "if [ \"\$EUID\" -ne 0 ]; then"
    echo 'echo "$0 is not running as root. Try using sudo."'
    echo "fi"
    echo ""
    echo "docker pull nakkaya/emacs:latest"
    echo ""
    echo "if [ \$(docker ps -q -f name=emacsd) ]; then"
    echo "    docker stop emacsd"
    echo "    docker rm emacsd"
    echo "fi"
    echo ""
    echo "$cmd"
} > /usr/bin/emacsd

chmod +x /usr/bin/emacsd

echo -e "\nNext steps,"
echo -e "\nStart the container,"
echo -e " - /usr/bin/emacsd"
echo -e "\nAccess via,"
echo -e " - ssh core@localhost -p 9090"
echo -e " - http://localhost:9090\n\n"
