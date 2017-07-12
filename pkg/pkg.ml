#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "shell-9p" @@ fun _c ->
  Ok [
    Pkg.bin "src/shell_9p" ~dst:"shell9p";
  ]
