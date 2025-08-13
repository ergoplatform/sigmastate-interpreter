/** @type {import("jest").Config} */
const config = {
  transform: {}, // reduce non-cached test time by about 20x by disabling babel code transformation
  moduleDirectories: ["<rootDir>/node_modules"],
  moduleNameMapper: {
    "sigmastate-js/main": "<rootDir>/../sc/js/target/scala-2.13/sc-opt/main.js",
  },
};

module.exports = config;
