# Emacs 

![CI Status](https://github.com/nakkaya/emacs/actions/workflows/main.yml/badge.svg)

### Docker Setup

There is a Docker image built from this repository that contains Emacs
28 with all packages AOT compiled. Image runs as user `nakkaya` replace
the your username as required.

Run with display forwarded,

```
docker run --net=host \
       --env="DISPLAY" \
       --volume=$HOME:/storage \
       --volume=$HOME/.Xauthority:/home/nakkaya/.Xauthority:rw \
       -it nakkaya/emacs:latest emacs
```

There is also a sample `docker-compose.yml` file that will setup a
web based interface that can be used on remote machines in
`devops/docker/`. After `docker-compose up -d` Emacs can be accessed
at `127.0.0.1:8080`.


### Semi Automated Setup

Clone this repository,

	git clone git@github.com:nakkaya/emacs.git

Navigate to `devops/<your_os>` and run the provided install script.

### Manual Setup

Clone this repository,

	git clone git@github.com:nakkaya/emacs.git
	
Tell ```.emacs``` to load ```init.el```.

	(load-file "~/source/emacs/init.el")
