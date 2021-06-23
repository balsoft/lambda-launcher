{ lib, makeWrapper, lambda-launcher-unwrapped, libqalculate, wmctrl, librsvg
, plugins ? [ libqalculate wmctrl ] }:
lambda-launcher-unwrapped.overrideAttrs (old: {
  buildInputs = old.buildInputs ++ [ makeWrapper ];
  postInstall = ''
    wrapProgram $out/bin/lambda-launcher --prefix PATH : ${
      lib.makeBinPath plugins
    } --prefix LD_LIBRARY_PATH : ${librsvg}/lib
  '';
})
