# Semantic Code Editor

The semantic code editor is a REPL application that allows quick navigation within huge Java projects.
In contrast to classical text editors, it focuses on navigating between syntactical elements instead of lines of code.

## Try it out

```
docker run -v /path/to/your/source/code:/data -ti fboeller/semantic-code-editor
```

Starts the semantic code editor and allows the navigation through a Java source directory from your local machine.
Exchange `/path/to/your/source/code` with a path to your source directory.

## Example

Find all classes defining a field of the custom type `System`:

```
> list class (field && type "System")
1: class AppController
  1: field system: System
2: class AppModel
  1: field system1: System
  2: field system2: System
3: class AppTest
  1: field system: System
```

List all interfaces with their methods:

```
> list interface method
1: interface Dao
  1: method delete(E): void
  2: method find(Long): E
  3: method findAll(): List<E>
  4: method merge(E): E
  5: method persist(E): void
2: interface ModelService
  1: method findAllModels(): List<Model>
  2: method findAllSubmodels(): List<Submodel>
```

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
* `field`: A field definition
* `parameter`: A parameter of a method signature
* `extension`: A class extending another class
* `type`: The type of a field, the return type of a method or the type of a parameter
* `definition`: The definition of a type

### List command

The `list` command allows you to print a subset of the tree under your currently focused element.
It is usable in two variants:

* With an index path like `list 1.5`: It lists all elements of the fifth sub element of the first element of the last printed tree.
* With a selection condition like `list method parameter`: 
  It lists all parameters of all methods within the currently focused element.
  It accepts an arbitrary number of arguments, each representing a selection condition on a subsequent layer of the syntax tree beneath the currently focused element.
  Such a selection condition can be a simple element type such as `method` 
  or a more complex one matching sub strings of the name or type of an element such as `field && name "app" && type "App"`.
  Use the wildcard character `*` to represent an arbitrary element type.

#### Examples

Find the main method of the project:

```
> list class (method && name "main")
1: class App
  1: method main(String[]): void
```

List all methods returning a string:

```
> list class (method && type "String")
1: class BaseEntity
  1: method getName(): String
2: class Entity
  1: method getName(): String
  2: method toString(): String
```

List all parameters of a complex method:

```
> list class (method && name "myMethod") parameter
1: class ServiceImpl
  1: method myMethod(EntityDao,SubentityDao,ModelDao): void
    1: parameter entityDao: EntityDao
    2: parameter subentityDao: SubentityDao
    3: parameter modelDao: ModelDao
```

#### Shortcuts

The list command provides different shortcuts for quicker navigation.

* Use `list` instead of `list *`
* Use `list "App"` instead of `list (name "App")`

### Read command

The `read` command allows you to print an element in Java syntax.
In contrast to the `list` command, it also allows printing of method bodies.

As a parameter, it accepts an index path like `3.1`.

#### Examples

Read the currently focused element:

```
public class App { ... } > read
public class App { 
  private static final Logger LOGGER; 
  public static void main (String[] args) { ... }
  public static void initData () { ... }
  public static void queryData () { ... } 
}
```

Read the second sub element of the fourth element of the last printed tree:

```> read 4.2```

### Focus command

The `focus` command allows you to switch to a different element within your project.
In contrast to the `list` and `read` commands, this is a stateful operation.
It allows you to navigate deeper into your project and to focus only on a part of it.

#### Examples

Focus on the first sub element of the second element of the last printed tree:

```
1: class App
  1: method initData(): void
  2: method main(String[]): void
  3: method queryData(): void
2: class AppTest
  1: method tearDown(): void
  2: method test(): void
> focus 2.1
public void tearDown() { ... } > 
```

Focus the previously focused element:

```focus ..```

Focus the project element:

```focus /```

#### Shortcuts

Each indexed `focus` command can only be executed without mentioning the keyword `focus` explicitely.

* You can use `1` instead of `focus 1`.
* You can use `14.6` instead of `focus 14.6`.

## Build from source

The project is written in Haskell and uses `stack` as a build system.
There are two ways provided to build the project:

* With `stack` configured on the host to build inside of a `docker` container (recommended).
* Completely inside of a `docker` container

Both ways require `docker` to be installed on the host.
For both ways, no haskell compiler needs to be installed.
The second way has the advantage of not requiring `stack`, but is much slower since it does not utilize `stack`s caching abilities.

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
  This will build a docker container `fboeller/semantic-code-editor:latest`.
  It takes an aweful amount of time since it is requiring a huge docker container (~4GB) with a whole haskell build environment.
* Start the program with `docker run -v /path/to/your/source/code:/data -ti fboeller/semantic-code-editor` where `/path/to/your/source/code` is an absolute path to a directory on your host containing Java source code.
