{ pkgs, ... }:
{
  services.ollama = {
    enable = true;

    # Vulkan backend works on the Arc B580; there's no ROCm/CUDA path for Intel.
    package = pkgs.ollama-vulkan;

    # Bind on all interfaces but don't open the port on the public firewall --
    # only reachable via tailscale0, which is a trusted interface network-wide.
    host = "0.0.0.0";

    loadModels = [
      "llama3.1:8b"
    ];

    home = "/data/var/lib/ollama";

    environmentVariables = {
      # Hermes Agent requires a served context window of at least 64K
      # (its own system prompt + tool definitions need the room).
      # llama3.1:8b is natively trained for up to 128K, so this is real,
      # not just a reported number.
      OLLAMA_CONTEXT_LENGTH = "65536";
      # A real 64K KV cache at fp16 (~8GB) plus model weights (~5GB)
      # doesn't fit in 12GB VRAM -- q8_0 roughly halves the cache size.
      OLLAMA_KV_CACHE_TYPE = "q8_0";
    };
  };
}
