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

    vsce show github.copilot-chat --json | \
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
        "\(.version)\t\(.engine)"' | sort -r | head -1 | awk -F'\t' '{ printf "Extension version: %s\nEngine constraint: %s\n", $1, $2 }'
  '';
}
