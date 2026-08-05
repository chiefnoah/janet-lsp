{
  commit,
  fetchFromGitHub,
  git,
  janet,
  jpm,
  lib,
  makeWrapper,
  src,
  stdenv,
  version,
}:

let
  spork = fetchFromGitHub {
    owner = "janet-lang";
    repo = "spork";
    rev = "dbfc90d4c86fa54a74c56d406517605e49569a8b";
    hash = "sha256-NbQ7Ghu1VINlkMAzQfVUQRtLQ3sUOYjMnrfFV2H1ZNo=";
  };

  cmd = fetchFromGitHub {
    owner = "ianthehenry";
    repo = "cmd";
    rev = "b4308de361d0f90dd96cc0f9a8dc6881e0e851c6";
    hash = "sha256-FG11D8/+ZDHudi6PXy0tKFYCbHUyy0KOqMoZJyFCm9s=";
  };

  judge = fetchFromGitHub {
    owner = "ianthehenry";
    repo = "judge";
    rev = "10754df781b34068e05291951db786933ec6b681";
    hash = "sha256-1zvr1WsllZFeKPb4EgdvTpp67lo1T/M8dEXGNHzOHj0=";
  };
in
stdenv.mkDerivation {
  pname = "janet-lsp";
  inherit src version;

  nativeBuildInputs = [
    janet
    jpm
    makeWrapper
  ];

  postPatch = ''
    sed -i '10,15c\(def commit "${commit}")' src/server-meta.janet
  '';

  buildPhase = ''
    runHook preBuild

    tree="$PWD/jpm-tree"
    mkdir -p "$tree"

    cp -R ${spork} spork
    cp -R ${cmd} cmd
    chmod -R u+w spork cmd

    (cd spork && jpm --tree="$tree" install)
    (cd cmd && jpm --tree="$tree" install)

    runHook postBuild
  '';

  doCheck = true;

  nativeCheckInputs = [ git ];

  checkPhase = ''
    runHook preCheck

    checkTree="$PWD/check-tree"
    cp -R "$tree" "$checkTree"
    cp -R ${judge} judge
    chmod -R u+w judge
    (cd judge && jpm --tree="$checkTree" install)

    export TMPDIR=/tmp
    export XDG_CACHE_HOME="$TMPDIR/cache"
    mkdir -p "$XDG_CACHE_HOME"
    JANET_PATH="$checkTree/lib" janet "$checkTree/bin/judge" \
      test/test-main.janet \
      test/test-lookup.janet \
      test/test-parser.janet \
      test/test-integration.janet

    runHook postCheck
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/lib/janet-lsp" "$out/bin"
    cp -R src libs "$out/lib/janet-lsp"
    cp -R "$tree/lib" "$out/lib/janet-lsp/modules"

    makeWrapper ${lib.getExe janet} "$out/bin/janet-lsp" \
      --set JANET_PATH "$out/lib/janet-lsp/modules" \
      --add-flags "$out/lib/janet-lsp/src/main.janet"

    runHook postInstall
  '';

  doInstallCheck = true;

  installCheckPhase = ''
    "$out/bin/janet-lsp" --version | grep -F "Janet LSP v${version}-${commit}"
  '';

  meta = {
    description = "Language server for the Janet programming language";
    homepage = "https://github.com/chiefnoah/janet-lsp";
    license = lib.licenses.mit;
    mainProgram = "janet-lsp";
    maintainers = [
      {
        email = "noah@packetlost.dev";
        github = "chiefnoah";
        githubId = 3588683;
        name = "Noah Pederson";
      }
    ];
    platforms = jpm.meta.platforms;
  };
}
