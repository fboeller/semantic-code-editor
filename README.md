# Semantic Code Editor

The semantic code editor is a REPL application that allows quick navigation within huge Java projects.
In contrast to classical text editors, it focuses on navigating between syntactical elements instead of lines of code.

## Example

Find all classes defining a field of the custom type `System`:

```> list class (variable && name "System")```

List all interfaces with their methods:

```> list interface method```

## Usage

The application currently allows the usage of three commands:

* `list` for listing desired elements of your project
* `focus` for switching the focus to a different part of your project
* `read` for printing the source code of an element of your project

Furthermore, the application distinguishes between different types of elements:

* `class`: A class definition
* `interface`: An interface definition
* `enum`: An enum definition
* `method`: A method definition
* `variable`: A field definition
* `parameter`: A parameter of a method signature
* `extension`: A class extending another class
* `type`: The type of a field, the return type of a method or the type of a parameter
* `definition`: The definition of a type

### List command

The `list` command allows you to print a subset of the tree under your currently focused element.
It is usable in two variants:

* With an index path like `list 1.5`: It lists all elements of the fifth sub element of the first element of the last printed tree.
* With a selection condition like `list method parameter`: It lists all parameters of all methods within the currently focused element.

#### Examples

Find the main method of the project:

```
list class (method && name "main")
1: class App
        1: method main(String[]): void
```

List all methods returning a string:

```> list class (method && type "String")```

List all parameters of a complex method:

```> list class (method && name "myMethod") parameter```

#### Shortcuts

The list command provides different shortcuts for quicker navigation.

* Use `list` instead of `list *`
* Use `list "App"` instead of `list (name "App")`

### Read command

The `read` command allows you to print an element in Java syntax.
In contrast to the `list` command, it also allows printing of method bodies.

As a parameter, it only accepts an index path like `3.1`.

#### Examples

Read the currently focused element:

```> read```

Read the second sub element of the fourth element of the last printed tree:

```> read 4.2```

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
* Run `./build.sh`.
  This will build a docker container `sce:latest`.
  It takes an aweful amount of time since it is requiring a huge docker container (~4GB) with a whole haskell build environment.
* Start the program with `docker run -ti -v <srcdir>:/data sce:latest` where `<srcdir>` is an absolute path to a directory on your host containing Java source code.
