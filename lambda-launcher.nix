{ mkDerivation, aeson, async, base, bytestring, data-default-class
, directory, filepath, gi-gdk, gi-glib, gi-gobject, gi-gtk
, gi-gtk-declarative, gi-gtk-declarative-app-simple, haskell-gi
, haskell-gi-base, process, req, stdenv, text, vector
}:
mkDerivation {
  pname = "lambda-launcher";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bytestring data-default-class directory filepath
    gi-gdk gi-glib gi-gobject gi-gtk gi-gtk-declarative
    gi-gtk-declarative-app-simple haskell-gi haskell-gi-base process
    req text vector
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
