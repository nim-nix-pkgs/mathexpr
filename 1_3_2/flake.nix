{
  description = ''MathExpr - pure-Nim mathematical expression evaluator library'';

  inputs.flakeNimbleLib.owner = "riinr";
  inputs.flakeNimbleLib.ref   = "master";
  inputs.flakeNimbleLib.repo  = "nim-flakes-lib";
  inputs.flakeNimbleLib.type  = "github";
  inputs.flakeNimbleLib.inputs.nixpkgs.follows = "nixpkgs";
  
  inputs.src-mathexpr-1_3_2.flake = false;
  inputs.src-mathexpr-1_3_2.owner = "Yardanico";
  inputs.src-mathexpr-1_3_2.ref   = "refs/tags/1.3.2";
  inputs.src-mathexpr-1_3_2.repo  = "nim-mathexpr";
  inputs.src-mathexpr-1_3_2.type  = "github";
  
  outputs = { self, nixpkgs, flakeNimbleLib, ...}@deps:
  let 
    lib  = flakeNimbleLib.lib;
    args = ["self" "nixpkgs" "flakeNimbleLib" "src-mathexpr-1_3_2"];
  in lib.mkRefOutput {
    inherit self nixpkgs ;
    src  = deps."src-mathexpr-1_3_2";
    deps = builtins.removeAttrs deps args;
    meta = builtins.fromJSON (builtins.readFile ./meta.json);
  };
}