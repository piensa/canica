{ mkDerivation, aeson, base, bytestring, filepath, fsnotify, hint
, http-types, hylogen, lib, process, text, vector-space, wai, warp
, websockets
}:
mkDerivation {
  pname = "hylide";
  version = "0.1.5.1";
  sha256 = "0ryzhbmwrg173lmzyl8a77qnqp11maxcn72y1x0hn8mabj8js3hn";
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base hylogen vector-space ];
  executableHaskellDepends = [
    aeson base bytestring filepath fsnotify hint http-types hylogen
    process text wai warp websockets
  ];
  homepage = "https://github.com/sleexyz/hylogen";
  description = "WebGL live-coding environment for writing shaders with Hylogen";
  license = lib.licenses.mit;
}
