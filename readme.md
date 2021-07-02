# Emacs 

![CI Status](https://github.com/nakkaya/emacs/actions/workflows/main.yml/badge.svg)

### Docker Setup

There is a Docker image built from this repository that contains Emacs
28 with all packages AOT compiled. Image runs as user `nakkaya` replace
your username as required.

There are sample `docker-compose.yml` files in `devops/docker/`. That
will launch a web based interface that can be used on remote machines
and another that will launch Emacs in GUI mode with X forwarded If you
have `python` `invoke` installed these can be launched using,
    
    # For Web Interface (At 127.0.0.1:8080)
    invoke up
    # For GUI Interface
    invoke gui

### Semi Automated Setup

Clone this repository,

	git clone git@github.com:nakkaya/emacs.git

Navigate to `devops/<your_os>` and run the provided install script.

### Manual Setup

Clone this repository,

	git clone git@github.com:nakkaya/emacs.git
	
Tell ```.emacs``` to load ```init.el```.

	(load-file "~/source/emacs/init.el")
