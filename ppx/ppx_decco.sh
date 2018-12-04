#!/usr/bin/env node
const { spawn } = require("child_process");
const path = require("path");

const child = spawn(
    path.resolve(__dirname, "../_build/default/.ppx/ppx_decco/ppx.exe"),
    [ "--as-ppx", ...process.argv.slice(2) ],
    { stdio: "inherit" }
);
