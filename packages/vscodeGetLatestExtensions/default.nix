{ pkgs, ... }:
pkgs.writeShellApplication {
  name = "vscode-get-latest-extensions";

  runtimeInputs = [
    pkgs.jq
    pkgs.vsce
  ];

  text = ''
    if [[ $# -lt 2 ]]; then
      echo "Usage: $0 <publisher> <extension> [vscode-version]" >&2
      exit 1
    fi

    EXT_ORG="$1"
    EXT_NAME="$2"
    VSCODE_VERSION="''${3:-${pkgs.lib.versions.majorMinor pkgs.vscode.version}}"

    echo "Using VSCode version: $VSCODE_VERSION" >&2
    echo "Checking extension: $EXT_ORG.$EXT_NAME" >&2

    VERSION_INFO=$(vsce show "$EXT_ORG.$EXT_NAME" --json | \
      jq -r '
        .versions[]
        | {
          version,
          engine: (
            .properties[] | select(.key == "Microsoft.VisualStudio.Code.Engine") | .value
          ),
          isPrerelease: (
            [.properties[] | select(.key == "Microsoft.VisualStudio.Code.PreRelease") | .value][0] // "false"
          )
        }
        | select(.isPrerelease != "true")
        | "\(.version)\t\(.engine)"
      ' | awk -F'\t' -v vs="$VSCODE_VERSION" '
        function parse(v,   a, n) {
          n = split(v, a, /[.-]/);
          return a;
        }

        BEGIN {
          n = split(vs, vparts, /\./);
          v_major = vparts[1]; v_minor = vparts[2]; v_patch = (n >= 3 ? vparts[3] : 0);
        }

        {
          version = $1;
          engine = $2;

          if (match(engine, /^\^([0-9]+)\.([0-9]+)\.([0-9]+)/, m)) {
            e_major = m[1]; e_minor = m[2]; e_patch = m[3];

            if (v_major == e_major && v_minor >= e_minor) {
              print version "\t" engine;
            }
          }
        }' | sort -rV | head -1)

    if [[ -z "$VERSION_INFO" ]]; then
      echo "❌ No compatible version of $EXT_ORG.$EXT_NAME found for VSCode $VSCODE_VERSION." >&2
      exit 2
    fi

    EXT_VERSION=$(echo "$VERSION_INFO" | cut -f1)
    ENGINE_CONSTRAINT=$(echo "$VERSION_INFO" | cut -f2)

    VSIX_URL="https://marketplace.visualstudio.com/_apis/public/gallery/publishers/$EXT_ORG/vsextensions/$EXT_NAME/$EXT_VERSION/vspackage"
    echo "Fetching VSIX from: $VSIX_URL" >&2

    HASH_BASE32=$(nix-prefetch-url --type sha256 "$VSIX_URL")
    HASH_BASE64=$(nix hash convert --hash-algo sha256 "$HASH_BASE32")

    echo >&2
    echo "Extension version: $EXT_VERSION" >&2
    echo "Engine constraint: $ENGINE_CONSTRAINT" >&2
    echo "Prefetched SHA256: sha256-$HASH_BASE64" >&2
    echo >&2
    echo "Pre-formatted nix code to build $EXT_ORG.$EXT_NAME for vscode $VSCODE_VERSION:" >&2
    echo >&2

    cat <<EOF
    # This file is automatically updated by the output from
    # nix run github:etu/nixconfig#vcodeGetLatestExtensions $EXT_ORG $EXT_NAME
    #
    # Store page: https://marketplace.visualstudio.com/items?itemName=$EXT_ORG.$EXT_NAME
    { pkgs, ... }:
    pkgs.vscode-utils.extensionFromVscodeMarketplace {
      publisher = "$EXT_ORG";
      name = "$EXT_NAME";
      version = "$EXT_VERSION";
      sha256 = "$HASH_BASE64";
    }
    EOF
  '';
}
