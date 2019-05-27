{ lib, makeWrapper, lambda-launcher-unwrapped, libqalculate, wmctrl
, plugins ? [ libqalculate wmctrl ] }:
lambda-launcher-unwrapped.overrideAttrs (old: {
  buildInputs = old.buildInputs ++ [makeWrapper];
  postInstall = ''
    makeWrapper $out/bin/lambda-launcher $wrapperfile --prefix PATH : ${
      lib.makeBinPath plugins
    }
  '';
})
