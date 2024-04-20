let

pkgs = import <nixpkgs> {};

zig = pkgs.stdenv.mkDerivation {
    name = "zig";
    src = fetchTarball (
        if (pkgs.system == "x86_64-linux") then {
            url = "https://ziglang.org/download/0.12.0/zig-linux-x86_64-0.12.0.tar.xz";
            sha256 = "11p6plvcmhgsx7k2zbs766v6iih0qg8njwb8n29kqlhgq0csivvf";
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
    ];
    buildInputs = [
    ];
}