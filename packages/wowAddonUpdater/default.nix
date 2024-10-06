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
        "simulationcraft/simc-addon"
    )
    DETAILS_REPO="Tercioo/Details-Damage-Meter"

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

    # Function to get the latest tag URL for Details! using GitHub CLI
    get_latest_details_tag_url() {
        REPO="$1"
        LATEST_TAG=$(curl -s "https://github.com/$REPO/tags.atom" | xq -r '.feed.entry[0].title')

        if test -z "$LATEST_TAG" || test "$LATEST_TAG" == "null"; then
            echo "Failed to fetch latest tag for $REPO" >&2
            return 1
        fi

        echo "https://github.com/$REPO/archive/refs/tags/$LATEST_TAG.zip"
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

        # Download Details! separately
        DETAILS_URL=$(get_latest_details_tag_url "$DETAILS_REPO")
        echo "Downloading Details! from: $DETAILS_URL"
        curl -L -o "$TEMP_DIR/$(basename "$DETAILS_URL")" "$DETAILS_URL" || { echo "Failed to download Details!"; exit 1; }
    }

    # Step 3: Move old addons to history
    step_3_move_old_addons_to_history() {
        HISTORY_TARGET="$HISTORY_DIR/$(date +"%Y-%m-%d_%H:%M:%S")"

        mkdir -p "$HISTORY_TARGET"

        echo "Moving old addons to history..."
        mv "$WOW_ADDON_DIR"/* "$HISTORY_TARGET/" || { echo "Failed to move old addons to history"; exit 1; }
    }

    # Step 4: Unpack new addons
    step_4_unpacking_new_addons() {
        echo "Unpacking new addons..."
        for ZIP_FILE in "$TEMP_DIR"/*.zip; do
            if test "$ZIP_FILE" == "$TEMP_DIR/Details"*; then
                # Special handling for Details!
                DETAILS_TEMP_DIR="$TEMP_DIR/details_temp"
                mkdir -p "$DETAILS_TEMP_DIR"
                unzip -q "$ZIP_FILE" -d "$DETAILS_TEMP_DIR" || { echo "Failed to unzip Details!"; exit 1; }
                DETAILS_DIR=$(find "$DETAILS_TEMP_DIR" -maxdepth 1 -type d -name "Details-Damage-Meter-*")
                mv "$DETAILS_DIR/plugins"/* "$WOW_ADDON_DIR/" || { echo "Failed to move Details plugins"; exit 1; }
                mv "$DETAILS_DIR" "$WOW_ADDON_DIR/Details" || { echo "Failed to move Details folder"; exit 1; }
                rm -rf "$DETAILS_TEMP_DIR"
            else
                unzip -o "$ZIP_FILE" -d "$WOW_ADDON_DIR" || { echo "Failed to unzip $ZIP_FILE"; exit 1; }
            fi
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
