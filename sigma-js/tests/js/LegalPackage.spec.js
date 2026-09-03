const fs = require("fs");
const path = require("path");

const packageRoot = path.resolve(__dirname, "../..");
const repositoryRoot = path.resolve(packageRoot, "..");

function readJson(file) {
  return JSON.parse(fs.readFileSync(file, "utf8"));
}

function canonicalText(file) {
  return fs.readFileSync(file, "utf8").replace(/\r\n/g, "\n");
}

describe("npm legal package", () => {
  const legalFiles = ["LICENSE", "LICENSE-APACHE", "NOTICE"];

  test("declares the licenses of the compiled Scala.js bundle", () => {
    const manifest = readJson(path.join(packageRoot, "package.json"));
    const lock = readJson(path.join(packageRoot, "package-lock.json"));

    expect(manifest.license).toBe("MIT AND Apache-2.0");
    expect(lock.packages[""].license).toBe(manifest.license);
    for (const file of legalFiles) {
      expect(manifest.files).toContain(file);
    }
  });

  test("copies the Scala.js bundle with the cross-platform shell helper", () => {
    const manifest = readJson(path.join(packageRoot, "package.json"));

    expect(manifest.scripts["copy-output"]).toBe(
      "shx mkdir -p ./dist/ && shx cp -r ../sc/js/target/scala-2.13/sc-fastopt/* ./dist/"
    );
  });

  test("ships the repository legal texts without semantic drift", () => {
    for (const file of legalFiles) {
      expect(canonicalText(path.join(packageRoot, file))).toBe(
        canonicalText(path.join(repositoryRoot, file))
      );
    }
    expect(canonicalText(path.join(packageRoot, "LICENSE-APACHE"))).toContain(
      "APPENDIX: How to apply the Apache License to your work."
    );
  });
});
