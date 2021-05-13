const path = require("path");
const fs = require("fs");

const installBinary = (binary) => {
  const source = path.join(__dirname, binary);
  if (fs.existsSync(source)) {
    const target = path.join(__dirname, "ppx")
    fs.renameSync(source, target)
    // it should be executable in the bundle, but just in case
    fs.chmodSync(target, 0777)
  } else {
    // assume we're in dev mode - nothing will break if the script
    // isn't overwritten, it will just be slower
  }
}


switch (process.platform) {
  case "linux":
    installBinary("ppx-linux.exe")
    break;
  case "darwin":
    installBinary("ppx-osx.exe")
    break;
  case "win32":
  default:
    // This won't break the installation because the `ppx` shell script remains
    // but that script will throw an error in this case anyway
    console.warn(`No release available for "${process.platform}"`)
    process.exit(1)
}
