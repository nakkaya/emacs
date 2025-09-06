# Emacs 

![CI Status](https://github.com/nakkaya/emacs/actions/workflows/main.yml/badge.svg)

### Docker Setup

There is a Docker image built from this repository that contains Emacs
30. Image runs as user `core` replace your username as required.

    docker pull nakkaya/emacs:latest

You can run it using,

    docker run \
	  --restart=always \
	  --name emacsd \
	  --detach \
	  -p 9090:9090/tcp \
	  nakkaya/emacs

or use the provided install script,

    curl https://raw.githubusercontent.com/nakkaya/emacs/master/install.sh -o install.sh
	# inspect the file
	sudo bash install.sh

It will guide you through all the options.

    # Connect using
    xpra attach tcp://127.0.0.1:9090 --window-close=disconnect
    # or
    chrome --app=http://127.0.0.1:9090

Service ports/paths used by the image when enabled,

 - Xpra: `9090`
 - SSH: `9090`
 - WebDAV: `/disk`
 - Jupyter: `/jupyter` (When enabled.)
 - PGAdmin: `/pgadmin` (When enabled.)
 - VScode: `/vscode` (When enabled.)

### Semi Automated Setup

Clone this repository,

	git clone git@github.com:nakkaya/emacs.git

Navigate to `devops/<your_os>` and run the provided install script.

### Manual Setup

Clone this repository,

	git clone git@github.com:nakkaya/emacs.git
	
Tell ```.emacs``` to load ```init.el```.

	(load-file "~/source/emacs/init.el")
