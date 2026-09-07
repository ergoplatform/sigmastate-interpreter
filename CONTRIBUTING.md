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

By default SBT uses Scala 2.13.18 for compilation and running tests. The JVM build
supports Scala 2.11.12, 2.12.21, 2.13.18, and 3.3.8. To build and test with Scala 3:

```shell
$ sbt
sbt:sigma-state> ++3.3.8
sbt:sigma-state> compile
sbt:sigma-state> test
```

You can also run SBT commands for all Scala versions at once:

```shell
$ sbt
sbt:sigma-state> +compile
sbt:sigma-state> +test
```

The Scala 3 build uses `-source:3.0-migration`. SBT applies this setting automatically.

The Scala.js build supports Scala 2.13.18 and 3.3.8 and requires Node.js and Yarn.
To test all six modules and the exported JavaScript API with Scala 3, run from
the repository root:

```shell
$ npm ci --prefix sigma-js
$ sbt '++3.3.8' coreJS/test dataJS/test interpreterJS/test parsersJS/test sdkJS/test scJS/test scJS/fastOptJS
$ cd sigma-js
$ SIGMA_JS_SCALA_TARGET=3.3.8 npm test -- --runInBand
```

`SIGMA_JS_SCALA_TARGET` selects the SBT target directory for the JavaScript API
tests. Leave it unset to use the default Scala 2.13 output; the Scala 3 target
directory uses the full version `3.3.8`.

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
