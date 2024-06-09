let

pkgs = import <nixpkgs> {};

zig = pkgs.stdenv.mkDerivation {
    name = "zig";
    src = fetchTarball (
        if (pkgs.system == "x86_64-linux") then {
            url = "https://ziglang.org/download/0.13.0/zig-linux-x86_64-0.13.0.tar.xz";
            sha256 = "01cvjk26ipz54q7dpp4669akh11aimw5zjq1chx3fh63aq2b02s2";
        } else
        throw ("Unknown system " ++ pkgs.system)
    );
    dontConfigure = true;
    dontBuild = true;
    dontStrip = true;
    installPhase = ''
    mkdir -p $out
    mv ./* $out/
    mkdir -p $out/bin
    mv $out/zig $out/bin
    '';
};

in

pkgs.mkShell rec {
    nativeBuildInputs = [
        zig
        pkgs.wabt
        pkgs.honggfuzz
    ];
    buildInputs = [
    ];
}