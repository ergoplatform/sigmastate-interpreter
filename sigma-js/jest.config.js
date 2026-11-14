// SBT uses the full Scala 3 version in its target directory; Scala 2 defaults to 2.13.
const scalaTarget = process.env.SIGMA_JS_SCALA_TARGET || "2.13";

/** @type {import("jest").Config} */
const config = {
  transform: {}, // reduce non-cached test time by about 20x by disabling babel code transformation
  moduleDirectories: ["<rootDir>/node_modules"],
  moduleNameMapper: {
    "sigmastate-js/main":
      `<rootDir>/../sc/js/target/scala-${scalaTarget}/sc-fastopt/main.js`,
  },
};

module.exports = config;
