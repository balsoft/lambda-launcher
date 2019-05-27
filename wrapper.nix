{ lib, lambda-launcher-unwrapped, libqalculate, wmctrl, plugins ? [ libqalculate wmctrl ] }: 
lambda-launcher-unwrapped.overrideAttrs (_: {
  postInstall = ''
    makeWrapper $out/bin/lambda-launcher $wrapperfile --prefix PATH : ${lib.makeBinPath plugins}
  '';
})
