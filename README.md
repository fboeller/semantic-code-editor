# Semantic Code Editor

## Installation

There is currently no other way to install this application than building from source.

## Build from source

The project is written in Haskell and uses `stack` as a build system.
There are two ways provided to build the project:

* With `stack` on the host configured to build inside of a `docker` container (recommended).
* Completely inside of a `docker` container

Both ways require `docker` to be installed on the host.
Also, both ways do have the advantage that no haskell compiler needs to be installed on the host.
While the second way does not require `stack` to be installed on the host, it is much slower since it does not utilize `stack`s caching abilities.

### Build with `stack` on the host

* Install `stack` on your host. 
  On most Linux distributions, this is best done via ```curl -sSL https://get.haskellstack.org/ | sh``` since the package repositories usually do not contain the latest stable version.
  Refer to <https://docs.haskellstack.org/en/stable/install_and_upgrade/> for more details.
* Install `docker` on your host.
  Refer to <https://docs.docker.com/install/> for more details.
* Clone this repository `git clone git@github.com:fboeller/semantic-code-editor.git`.
* Change the working directory to the cloned repository `semantic-code-editor`.
* Run `stack setup`.
* Run `stack build`.
* Run `stack run`.
  This will run the application.
  For further runs or after changes to the source, this command is sufficient and the other commands do not need to be repeated.

### Build inside of a `docker` container

* Install `docker` on your host.
  Refer to <https://docs.docker.com/install/> for more details.
* Clone this repository `git clone git@github.com:fboeller/semantic-code-editor.git`.
* Change the working directory to the cloned repository `semantic-code-editor`.
* Run `docker run .`.
  This will take an aweful amount of time since it is using a huge docker container (~4GB) with a whole haskell build environment.
