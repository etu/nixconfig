_: {
  disko.devices = {
    disk = {
      nvme0n1 = {
        type = "disk";
        device = "/dev/nvme0n1";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              size = "1024M";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [
                  "defaults"
                  "noexec"
                  "noauto"
                  "x-systemd.automount"
                ];
              };
            };
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "zroot";
              };
            };
          };
        }; # END disk.nvme0n1.content
      }; # END disk.nvme0n1
      nvme1n1 = {
        type = "disk";
        device = "/dev/nvme1n1";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              size = "1024M";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot-fallback";
                mountOptions = [
                  "defaults"
                  "noexec"
                  "noauto"
                  "x-systemd.automount"
                ];
              };
            };
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "zroot";
              };
            };
          };
        }; # END disk.nvme1n1.content
      }; # END disk.nvme1n1
    }; # END disk
    nodev."/" = {
      fsType = "tmpfs";
      mountOptions = [
        "size=64G"
        "defaults"
        "mode=755"
      ];
    };
    zpool = {
      zroot = {
        type = "zpool";

        # Make a mirror pool
        mode.topology = {
          type = "topology";
          vdev = [
            {
              mode = "mirror";
              members = [
                "nvme0n1"
                "nvme1n1"
              ];
            }
          ];
        };

        mountpoint = null;
        rootFsOptions = {
          compression = "lz4"; # To enable file system compression.
          mountpoint = "none"; # Disable ZFS automatic mounting.
          atime = "off"; # Disable writing access time.
          acltype = "posixacl"; # This is more or less required for certain things to not break.
          xattr = "sa"; # Improve performance of certain extended attributes.

          # These should work under datasets."name".options.{}
          encryption = "aes-256-gcm";
          keyformat = "passphrase";
          keylocation = "file:///tmp/secret.key";
        };

        options = {
          ashift = "12"; # Use 4K sectors on the drive, otherwise you can get really bad performance.
        };

        datasets = {
          "local/nix" = {
            type = "zfs_fs";
            mountpoint = "/nix";
            options.mountpoint = "legacy";
          };
          "local/var-log" = {
            type = "zfs_fs";
            mountpoint = "/var/log";
            options.mountpoint = "legacy";
          };
          "local/var-lib-docker" = {
            type = "zfs_fs";
            options.mountpoint = "/var/lib/docker";
          };
          "local/data" = {
            type = "zfs_fs";
            mountpoint = "/data/local";
            options.mountpoint = "legacy";
          };
          "safe/data" = {
            type = "zfs_fs";
            mountpoint = "/data";
            options.mountpoint = "legacy";
          };
          "safe/home" = {
            type = "zfs_fs";
            mountpoint = "/data/home";
            options.mountpoint = "legacy";
          };
        };
      }; # END zpool.zroot
    }; # END zpool
  };
}
