# Contributing 

## Building

Clone the repository.

```shell
$ git clone git@github.com:ScorexFoundation/sigmastate-interpreter.git
$ cd sigmastate-interpreter
$ sbt test
```

Then you can compile the library with SBT and run tests.

```shell
$ sbt
sbt:sigma-state> compile
sbt:sigma-state> test
```

By default SBT uses Scala 2.12 for compilation and running tests. To compile for Scala 2.13 use the following commands:

```shell
$ sbt
sbt:sigma-state> ++2.13.16
sbt:sigma-state> compile
sbt:sigma-state> test
```

You can also run SBT commands for all Scala versions at once:

```shell
$ sbt
sbt:sigma-state> +compile
sbt:sigma-state> +test
```

To run specific test suite use the following command:

```shell
sbt:sigma-state> testOnly <full test class name>
```

## Releasing

### Setup GPG key

Follow [instructions](https://central.sonatype.org/publish/requirements/gpg/) to set up GPG key.
You will need:
- create a GPG key pair;
- publish your public key to a public key server;

### Check Sonatype credentials
Try to login to Nexus Repository Manager with your credentials [here](https://oss.sonatype.org/#welcome)

### Using Sonatype with SBT

Follow [instructions](https://www.scala-sbt.org/release/docs/Using-Sonatype.html) to set up Sonatype with SBT.
You will also need:
- [how to publish a release](https://docs.scala-lang.org/overviews/contributors/index.html#publish-a-release)
- [sbt-sonatype plugin](https://github.com/xerial/sbt-sonatype)
- [sbt-dynver plugin](https://github.com/sbt/sbt-dynver)

### Publishing release
This can be done manually or automatically by Github Actions.

#### Manual publishing
To publish release to Sonatype, use the following:
```
$sbt
sbt:sigma-state> +publishSigned
sbt:sigma-state> sonatypeBundleRelease
```

#### Automatic publishing
To publish release version to Sonatype, do the following:
- make a tag with version number `vX.Y.Z` (used by `sbt-dynver` to set `version` key);
- use the new tag to make a Github release, which triggers [`release.yml`](.github/workflows/release.yml) workflow and publishes release version to Sonatype;
