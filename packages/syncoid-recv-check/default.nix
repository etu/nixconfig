{ pkgs, ... }:
pkgs.writeShellApplication {
  name = "syncoid-recv-check";

  runtimeInputs = [
    pkgs.zfs
  ];

  text = ''
    # Base dataset to scan for interrupted syncoid receives
    BASE="zstorage/backups/current"

    echo "Scanning $BASE for interrupted receives..."
    echo ""

    found=0
    while IFS=$'\t' read -r dataset token; do
      if [[ "$token" != "-" ]]; then
        echo "FOUND: $dataset (has receive_resume_token)"
        echo "       Fix with: zfs receive -A '$dataset'"
        echo ""
        found=$((found + 1))
      fi
    done < <(zfs get -H -o name,value receive_resume_token -r "$BASE" 2>/dev/null)

    if [[ $found -eq 0 ]]; then
      echo "No interrupted receives found. All good!"
    else
      echo "$found interrupted receive(s) found."
    fi
  '';
}
