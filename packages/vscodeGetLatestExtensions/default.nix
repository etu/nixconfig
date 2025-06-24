{
  pkgs,
  lib,
  ...
}:
pkgs.writeShellApplication {
  name = "vscode-get-latest-extensions";

  runtimeInputs = [pkgs.jq pkgs.vsce];

  text = ''
    VSCODE_VERSION="''${1:-${pkgs.lib.versions.majorMinor pkgs.vscode.version}}"
    echo "Using VSCode version: $VSCODE_VERSION" >&2
    echo >&2
    echo "Copilot Chat extension (github.copilot-chat)" >&2

    # Get latest compatible version
    VERSION_INFO=$(vsce show github.copilot-chat --json | \
      jq -r --arg vs "$VSCODE_VERSION" '
        .versions[] |
        {
          version,
          engine: (
            .properties[] | select(.key == "Microsoft.VisualStudio.Code.Engine") | .value
          ),
          isPrerelease: (
            [.properties[] | select(.key == "Microsoft.VisualStudio.Code.PreRelease") | .value][0] // "false"
          )
        } |
        select((.engine | test($vs)) and (.isPrerelease != "true")) |
        "\(.version)\t\(.engine)"' | sort -r | head -1)

    EXT_VERSION=$(echo "$VERSION_INFO" | cut -f1)
    ENGINE_CONSTRAINT=$(echo "$VERSION_INFO" | cut -f2)

    # Construct VSIX download URL
    VSIX_URL="https://marketplace.visualstudio.com/_apis/public/gallery/publishers/github/vsextensions/copilot-chat/$EXT_VERSION/vspackage"
    echo "Fetching VSIX from: $VSIX_URL" >&2

    # Prefetch SHA256 and convert it to base64
    HASH_BASE32=$(nix-prefetch-url --type sha256 "$VSIX_URL")
    HASH_BASE64=$(nix hash convert --hash-algo sha256 "$HASH_BASE32")

    echo >&2
    echo "Extension version: $EXT_VERSION" >&2
    echo "Engine constraint: $ENGINE_CONSTRAINT" >&2
    echo "Prefetched SHA256: sha256-$HASH_BASE64" >&2
    echo >&2
    echo "Pre-formatted nix code to build copilot-chat for $VSCODE_VERSION:" >&2
    echo >&2

    # Output ready-to-paste Nix block
    cat <<EOF
    pkgs.vscode-utils.extensionFromVscodeMarketplace {
      publisher = "github";
      name = "copilot-chat";
      version = "$EXT_VERSION";
      sha256 = "$HASH_BASE64";
    }
    EOF
  '';
}
