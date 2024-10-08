{
  writeShellApplication,
  jq,
  python3Packages,
  unzip,
  ...
}:
writeShellApplication {
  name = "wowAddonUpdater";
  runtimeInputs = [
    jq
    python3Packages.yq
    unzip
  ];
  text = ''
    # Configuration
    WOW_ADDON_DIR="$HOME/.var/app/WowAddons"
    TEMP_DIR="$WOW_ADDON_DIR/.tmp"
    HISTORY_DIR="$WOW_ADDON_DIR/.history"
    GITHUB_REPOS=(
        "BigWigsMods/BigWigs"
        "BigWigsMods/LittleWigs"
    )
    MANAGED_ADDONS=("BigWigs" "LittleWigs")

    # Function to get the latest release URL using GitHub CLI
    get_latest_release_url() {
        REPO="$1"
        LATEST_RELEASE_URL=$(curl -s "https://api.github.com/repos/$REPO/releases" | jq -r '.[0].assets[] | select(.name | test("[a-zA-Z].*.zip")) | .browser_download_url')

        if test -z "$LATEST_RELEASE_URL"; then
            echo "Failed to fetch latest release URL for $REPO" >&2
            return 1
        fi

        echo "$LATEST_RELEASE_URL"
    }

    # Step 1: Make a temporary and history directory
    step_1_prepare_directories() {
        rm -rf "$TEMP_DIR" # Delete the $TEMP_DIR

        # Create necessary directories
        mkdir -p "$TEMP_DIR" "$HISTORY_DIR"
    }

    # Step 2: Download addons
    step_2_download_addons() {
        echo "Fetching and downloading latest addon releases..."
        for REPO in "''${GITHUB_REPOS[@]}"; do
            LATEST_URL=$(get_latest_release_url "$REPO")
            echo "Downloading from: $LATEST_URL"
            curl -L -o "$TEMP_DIR/$(basename "$LATEST_URL")" "$LATEST_URL" || { echo "Failed to download $REPO"; exit 1; }
        done
    }

    # Step 3: Move old managed addons to history
    step_3_move_old_addons_to_history() {
        HISTORY_TARGET="$HISTORY_DIR/$(date +"%Y-%m-%d_%H:%M:%S")"
        mkdir -p "$HISTORY_TARGET"

        echo "Moving old managed addons to history..."
        for ADDON in "''${MANAGED_ADDONS[@]}"; do
            if test -d "$WOW_ADDON_DIR/$ADDON"; then
                mv "$WOW_ADDON_DIR/$ADDON"* "$HISTORY_TARGET/" || { echo "Failed to move $ADDON to history"; exit 1; }
            fi
        done
    }

    # Step 4: Unpack new addons
    step_4_unpacking_new_addons() {
        echo "Unpacking new addons..."
        for ZIP_FILE in "$TEMP_DIR"/*.zip; do
            unzip -o "$ZIP_FILE" -d "$WOW_ADDON_DIR" || { echo "Failed to unzip $ZIP_FILE"; exit 1; }
        done
    }

    # Step 5: Move the zip files to the root directory
    step_5_move_zip_files() {
        mv "$TEMP_DIR"/*zip "$WOW_ADDON_DIR"
    }

    # Step 6: Garbage collect old history directories
    step_6_garbage_collect() {
        rm -rf "$TEMP_DIR"
        find "$HISTORY_DIR" -maxdepth 1 -mindepth 1 -type d | sort | head -n -5 | xargs rm -rf
    }

    step_1_prepare_directories
    step_2_download_addons
    step_3_move_old_addons_to_history
    step_4_unpacking_new_addons
    step_5_move_zip_files
    step_6_garbage_collect
  '';
}
