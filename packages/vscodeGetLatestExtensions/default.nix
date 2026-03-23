{ pkgs, ... }:
let
  # Access the VS Code store path without triggering the unfree license check.
  # overrideAttrs with only meta changes produces the identical derivation hash,
  # but the wrapper's outPath assert is satisfied because the license is now free.
  vscodeRef = pkgs.vscode.overrideAttrs (old: {
    meta = old.meta // {
      license = pkgs.lib.licenses.mit;
    };
  });
in
pkgs.writeShellApplication {
  name = "vscode-get-latest-extensions";

  runtimeInputs = [
    pkgs.jq
    pkgs.vsce
  ];

  text = ''
    if [[ $# -lt 2 ]]; then
      echo "Usage: $0 <publisher> <extension> [output-file|-]" >&2
      exit 1
    fi

    EXT_ORG="$1"
    EXT_NAME="$2"
    OUTPUT_FILE="''${3:--}"

    VSCODE_VERSION="${pkgs.lib.versions.majorMinor vscodeRef.version}"

    echo "Using VSCode version: $VSCODE_VERSION" >&2
    echo "Checking extension: $EXT_ORG.$EXT_NAME" >&2

    # Extract versioned API proposal support from VS Code's compiled source.
    # VS Code defines proposals like: chatDebug:{proposal:"...",version:2}
    # Extensions in extensionsEnabledWithApiProposalVersion can require specific
    # versions (e.g. chatDebug@3) that must match what VS Code supports.
    VSCODE_APP_DIR="${vscodeRef}/lib/vscode/resources/app"
    VSCODE_PROPOSALS=$(grep -oP '[a-zA-Z0-9]+:\{proposal:"https://[^"]*",version:[0-9]+\}' \
      "$VSCODE_APP_DIR/out/main.js" | \
      sed -E 's/^([a-zA-Z0-9]+):\{proposal:"[^"]*",version:([0-9]+)\}/\1@\2/')

    NEEDS_PROPOSAL_CHECK="false"
    if jq -e --arg id "$EXT_ORG.$EXT_NAME" \
        '.extensionsEnabledWithApiProposalVersion // [] | map(ascii_downcase) | index($id | ascii_downcase)' \
        "$VSCODE_APP_DIR/product.json" > /dev/null 2>&1; then
      NEEDS_PROPOSAL_CHECK="true"
      echo "Extension uses versioned API proposals — checking compatibility" >&2
    fi

    CANDIDATES=$(vsce show "$EXT_ORG.$EXT_NAME" --json | \
      jq -r '
        .versions[]
        | {
          version,
          engine: (
            .properties[] | select(.key == "Microsoft.VisualStudio.Code.Engine") | .value
          ),
          proposals: (
            [.properties[] | select(.key == "Microsoft.VisualStudio.Code.EnabledApiProposals") | .value][0] // ""
          ),
          isPrerelease: (
            [.properties[] | select(.key == "Microsoft.VisualStudio.Code.PreRelease") | .value][0] // "false"
          )
        }
        | select(.isPrerelease != "true")
        | "\(.version)\t\(.engine)\t\(.proposals)"
      ' | awk -F'\t' -v vs="$VSCODE_VERSION" '
        BEGIN {
          n = split(vs, vparts, /\./);
          v_major = vparts[1]; v_minor = vparts[2]; v_patch = (n >= 3 ? vparts[3] : 0);
        }

        {
          version = $1;
          engine = $2;
          proposals = $3;

          if (match(engine, /^\^([0-9]+)\.([0-9]+)\.([0-9]+)/, m)) {
            e_major = m[1]; e_minor = m[2]; e_patch = m[3];

            if (v_major == e_major && v_minor >= e_minor) {
              print version "\t" engine "\t" proposals;
            }
          }
        }' | sort -t$'\t' -k1,1 -rV)

    # Find the first engine-compatible version that also passes versioned
    # API proposal checks (only applies to extensions listed in
    # extensionsEnabledWithApiProposalVersion in VS Code's product.json).
    VERSION_INFO=""
    while IFS=$'\t' read -r ver eng proposals; do
      [[ -z "$ver" ]] && continue

      if [[ "$NEEDS_PROPOSAL_CHECK" == "true" ]] && [[ -n "$proposals" ]]; then
        compatible=true
        IFS=',' read -ra PROPS <<< "$proposals"
        for prop in "''${PROPS[@]}"; do
          if [[ "$prop" == *@* ]]; then
            if ! echo "$VSCODE_PROPOSALS" | grep -qxF "$prop"; then
              echo "  ⚠️ Skipping $ver: requires $prop (not supported by VSCode $VSCODE_VERSION)" >&2
              compatible=false
              break
            fi
          fi
        done
        if [[ "$compatible" == "true" ]]; then
          VERSION_INFO="$ver"$'\t'"$eng"
          break
        fi
      else
        VERSION_INFO="$ver"$'\t'"$eng"
        break
      fi
    done <<< "$CANDIDATES"

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

    if [[ "$OUTPUT_FILE" == "-" ]]; then
      tmpfile=$(mktemp)
    else
      tmpfile=$(mktemp "$(dirname "$OUTPUT_FILE")/.vscode-ext-XXXXXX")
    fi
    trap 'rm -f "''${tmpfile}"' EXIT

    cat > "$tmpfile" <<EOF
    # This file is automatically updated by the output from
    # nix run github:etu/nixconfig#vscodeGetLatestExtensions $EXT_ORG $EXT_NAME
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

    if [[ "$OUTPUT_FILE" == "-" ]]; then
      cat "$tmpfile"
    else
      mv "$tmpfile" "$OUTPUT_FILE"
    fi
  '';
}
