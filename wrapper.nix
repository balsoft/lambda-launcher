{ lib, makeWrapper, wrapGAppsHook, lambda-launcher-unwrapped, libqalculate, wmctrl, librsvg, pulseaudio
, plugins ? [ libqalculate wmctrl pulseaudio ] }:
lambda-launcher-unwrapped.overrideAttrs (old: {
  buildInputs = old.buildInputs ++ [ librsvg ];
  nativeBuildInputs = old.nativeBuildInputs ++ [ makeWrapper wrapGAppsHook ];
  postInstall = ''
    wrapProgram $out/bin/lambda-launcher --prefix PATH : ${
      lib.makeBinPath plugins
    } --prefix LD_LIBRARY_PATH : ${librsvg}/lib
  '';
})
