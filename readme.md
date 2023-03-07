# Emacs 

![CI Status](https://github.com/nakkaya/emacs/actions/workflows/main.yml/badge.svg)

### Docker Setup

There is a Docker image built from this repository that contains Emacs
28 with all packages AOT compiled. Image runs as user `core` replace
your username as required.

    docker push nakkaya/emacs:latest

You can run it using,

    docker run \
	  --privileged \
	  --security-opt="seccomp=unconfined" \
	  --restart=always \
	  --name emacsd \
	  --detach \
	  -p 2222:2222/tcp -p 4242:4242/tcp -p 9090:9090/tcp \
	  nakkaya/emacs

If you have `python` `invoke` installed you can run more complicated
commands,

    invoke docker                    # For bares bones version same as above
	invoke docker --with-gpu         # Run with GPUs attached
    invoke docker --with-host        # Run with host networking
    invoke docker --with-passwd 1234 # Set password for Web & SSH login

Either will launch a web based interface that can be used on remote
machines.

    # Connect using
    xpra attach tcp://127.0.0.1:9090 --window-close=disconnect
    # or
    chrome --app=http://127.0.0.1:9090

### Semi Automated Setup

Clone this repository,

	git clone git@github.com:nakkaya/emacs.git

Navigate to `devops/<your_os>` and run the provided install script.

### Manual Setup

Clone this repository,

	git clone git@github.com:nakkaya/emacs.git
	
Tell ```.emacs``` to load ```init.el```.

	(load-file "~/source/emacs/init.el")
