#!/usr/bin/env node
const { spawn } = require("child_process");
const fs = require("fs");
const path = require("path");

function rel(filePath) {
    return path.resolve(__dirname, filePath);
}

let ppxPath;
if(fs.existsSync(rel("_esy/default/build/default/.ppx/ppx_decco/ppx.exe"))) {
    ppxPath = "_esy/default/build/default/.ppx/ppx_decco/ppx.exe";
} else if(process.platform === "darwin") {
    ppxPath = "ppx-osx.exe";
} else {
    ppxPath = "ppx-linux.exe";
}

const child = spawn(
    path.resolve(__dirname, ppxPath),
    [ "--as-ppx", ...process.argv.slice(2) ],
    { stdio: "inherit" }
);
