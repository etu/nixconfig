#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3 python3Packages.requests
from concurrent.futures import ThreadPoolExecutor
import json
import os
import requests

EXTENSIONS = sorted([
    "browserpass-ce",
    "elasticvue",
    "multi-account-containers",
    "görans-hemmasnickrade-ordli",
    "https-everywhere",
    "privacy-badger17",
    "sidebery",
    "terms-of-service-didnt-read",
    "ublock-origin",
])

def index_ext(ext: str):
    print(f"Indexing {ext}...")

    resp = requests.get(f"https://addons.mozilla.org/api/v5/addons/addon/{ext}/").json()
    rel = resp["current_version"]

    if not rel["file"]["hash"].startswith("sha256:"):
        raise ValueError("Unhandled hash type")

    return {
        "pname": ext,
        "version": rel["version"],
        "addonId": resp["guid"],
        "url": rel["file"]["url"],
        "sha256": rel["file"]["hash"],
    }

if __name__ == "__main__":
    outfile = os.path.dirname(os.path.realpath(__file__)) + "/extensions.json"

    with ThreadPoolExecutor() as e:
        extensions = {ext: e.submit(index_ext, ext) for ext in EXTENSIONS}
        extensions = {k: v.result() for k, v in extensions.items()}

    with open(outfile, "w") as f:
        json.dump(extensions, f, indent=2)
