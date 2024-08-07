[![Cache 📝](https://github.com/etu/nixconfig/actions/workflows/cache.yml/badge.svg)](https://github.com/etu/nixconfig/actions/workflows/cache.yml)
[![Deploy 🚀](https://github.com/etu/nixconfig/actions/workflows/deploy.yml/badge.svg)](https://github.com/etu/nixconfig/actions/workflows/deploy.yml)
[![Format 🔎](https://github.com/etu/nixconfig/actions/workflows/format.yml/badge.svg)](https://github.com/etu/nixconfig/actions/workflows/format.yml)
[![Update ⬆️](https://github.com/etu/nixconfig/actions/workflows/update.yml/badge.svg)](https://github.com/etu/nixconfig/actions/workflows/update.yml)

# My NixOS configs

##  Files and Directories in this repository

### `data.nix`

This file doesn't contain any secrets at all, it's just miscellaneous
public data that I need to be able to access from many places.

It also contains public SSH keys for all the systems I have and care
about, but also for trusted users that should have access to accounts
on different systems. This becomes a central place to manage said keys
for said users.

### `hosts/`

#### `hosts/laptop-private-elis/`

Private laptop, deployed like a normal NixOS system using
`nixos-rebuild` to build new generations. ZFS snapshots are pushed
from this system to `server-main-elis` whenever this system is online.

#### `hosts/laptop-work-elis/`

Work laptop, deployed like a normal NixOS system using `nixos-rebuild`
to build new generations. ZFS snapshots are pushed from this system to
`server-main-elis` whenever this system is online.

#### `hosts/server-main-elis/`

Home file server, deployed using `deploy .#server-main-elis`. Also
used as build machines for the laptops. It's also my primary location
to store ZFS snapshots that I backup from all of the other systems. It
runs home assistant and some other things.

#### `hosts/server-sparv/`

On location server for http://speliarvika.se, will be used for LAN
cache among other things.

#### `hosts/vps06/`

System that runs Gitea, [ip.failar.nu](https://ip.failar.nu/), and a
Matrix home server among some other things. Deployed using `deploy
.#vps06`.

#### `hosts/live-iso/`

If you're adventurous and want to run a clone of my configuration from
a live-iso, it can be built locally:

```sh
nix build github:etu/nixconfig#iso
```

### `modules/`

This directory contains a whole bunch of modules that I've come up
with to make it easier for me to quickly configure multiple systems to
do similar things. So I've made my own modules with my own options.

Here's things like Emacs,
[home-manager](https://github.com/nix-community/home-manager), sway
configuration, the list goes on.

### `secrets.nix` and `secrets/`

This is the directory with real secrets managed with
[agenix](https://github.com/ryantm/agenix) which
[age](https://github.com/FiloSottile/age) encrypt files using the
public SSH keys for my users on my primary laptops and the public
SSH-key on the host of intended use. This way I can see, edit and
update encryption keys on my primary laptops and commit these files to
the repository. But then also the target system of intended use can
decrypt it with it's stateful private SSH key.
