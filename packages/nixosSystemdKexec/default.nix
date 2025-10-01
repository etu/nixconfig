{ pkgs, ... }:
pkgs.writeShellApplication {
  name = "nixos-systemd-kexec";

  runtimeInputs = [
    pkgs.fzf
    pkgs.kexec-tools
    pkgs.gawk
    pkgs.gnused
    pkgs.coreutils
  ];

  text = ''
    EFI_MOUNT_DIR=/boot
    ENTRIES_DIR=/boot/loader/entries
    DRY=0
    INJECT_CONSOLE=1
    LOGLEVEL=""
    NO_CONFIRM=0

    while [[ $# -gt 0 ]]; do
      case "$1" in
        --dry-run)
          DRY=1
          shift
          ;;
        --no-console)
          INJECT_CONSOLE=0
          shift
          ;;
        --loglevel)
          LOGLEVEL="$2"
          shift 2
          ;;
        --no-confirm)
          NO_CONFIRM=1
          shift
          ;;
        -h|--help)
          cat <<EOL
    Usage: nixos-kexec-picker.sh [--dry-run] [--no-console] [--loglevel N] [--no-confirm]

    Assumes all dependencies exist. Selects entry via fzf and jumps via kexec.
    Defaults: injects console=tty1. Confirms before jumping unless --no-confirm.
    Options:
      --dry-run     : Show what would be done without executing
      --no-console  : Don't add console=tty1 to kernel cmdline
      --loglevel N  : Set or replace loglevel in kernel cmdline
      --no-confirm  : Skip confirmation prompt before jumping
    EOL
          exit 0
          ;;
        *)
          echo "Unknown flag: $1" >&2
          exit 1
          ;;
      esac
    done

    list_entries() {
      shopt -s nullglob
      for f in "$ENTRIES_DIR"/nixos-generation-*.conf; do
        b=$(basename "$f")

        if [[ "$b" =~ nixos-generation-([0-9]+)-specialisation-([^.]+)\.conf$ ]]; then
          gen="''${BASH_REMATCH[1]}"; spec="''${BASH_REMATCH[2]}"
        elif [[ "$b" =~ nixos-generation-([0-9]+)\.conf$ ]]; then
          gen="''${BASH_REMATCH[1]}"; spec="-"
        else
          continue
        fi

        title=$(awk -F'= *' '/^title/ {print substr($0,index($0,"=")+1); exit}' "$f" 2>/dev/null || true)

        [[ -z "''${title:-}" ]] && title="generation $gen''${spec:+ (spec: $spec)}"

        printf "%s\t%s\t%s\t%s\n" "$gen" "$spec" "$title" "$f"
      done | sort -t$'\t' -k1,1nr -k2,2
    }

    choose_entry() {
      list_entries | \
        awk -F'\t' '{printf "%-4s  %-10s  %s\t%s\n",$1,($2=="-"?"base":$2),$3,$4}' | \
        fzf --prompt="Select generation/specialisation: " --with-nth=1..3 --delimiter=$'\t' --height=80% | \
        awk -F'\t' '{print $NF}'
    }

    add_or_replace_kv() {
      local line="$1" key="$2" val="$3"

      if grep -Eq "(^|[[:space:]])''${key}=" <<<"$line"; then
        echo "$line" | sed -E "s/(^|[[:space:]])''${key}=[^[:space:]]+/\1''${key}=''${val}/"
      else
        echo "$line ''${key}=''${val}"
      fi
    }

    ensure_console_tty1() {
      local line="$1"
      if grep -Eq '(^|[[:space:]])console=' <<<"$line"; then
        echo "$line"
      else
        echo "$line console=tty1"
      fi
    }

    kexec_from_entry() {
      local entry="$1"
      local linux initrd cmdline
      linux=''${EFI_MOUNT_DIR}$(awk '/^linux /{print $2; exit}'  "$entry")
      initrd=''${EFI_MOUNT_DIR}$(awk '/^initrd /{print $2; exit}' "$entry")
      cmdline=$(awk '/^options /{$1=""; sub(/^ /,""); print; exit}' "$entry")

      (( INJECT_CONSOLE )) && cmdline=$(ensure_console_tty1 "$cmdline")
      [[ -n "''${LOGLEVEL}" ]] && cmdline=$(add_or_replace_kv "$cmdline" "loglevel" "$LOGLEVEL")

      echo "Boot entry: $entry"
      echo "Kernel:     $linux"
      echo "Initrd:     $initrd"
      echo "Cmdline:    $cmdline"
      echo

      if (( ! NO_CONFIRM )); then
        read -r -p "Proceed with kexec to this kernel? (y/N) " ans
        [[ "$ans" =~ ^[Yy]$ ]] || { echo "Aborted."; exit 130; }
      fi

      if (( DRY )); then
        echo "[Dry-run] Would run:"
        echo "kexec -l '$linux' --initrd='$initrd' --command-line='$cmdline'"
        echo "systemctl kexec"
        exit 0
      fi

      kexec -l "$linux" --initrd="$initrd" --command-line="$cmdline"
      echo "Executing systemctl kexec now..."
      exec systemctl kexec
    }

    main() {
      entry=$(choose_entry)

      [[ -n "''${entry:-}" ]] || { echo "No selection made."; exit 130; }

      kexec_from_entry "$entry"
    }
    main
  '';

  meta.description = "Package to do kexec to other systemd-boot nixos generations";
}
