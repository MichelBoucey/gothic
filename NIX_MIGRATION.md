# Migration from Stack to Nix Flake

This document describes the migration from Stack to Nix Flake for the Gothic project.

## What's New

The project now includes a `flake.nix` file that provides:

1. **Package builds** using Cabal via Nix
2. **Development shells** with all necessary tools (GHC, cabal-install, HLS, ghcid)
3. **Multiple GHC version support** (GHC 8.10, 9.0, 9.2, 9.4, 9.6)
4. **Reproducible builds** across different machines

## Prerequisites

You need Nix with flake support enabled. If you don't have it:

```bash
# Install Nix
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

# Or enable flakes in your existing Nix installation by adding to ~/.config/nix/nix.conf:
experimental-features = nix-command flakes
```

## First-Time Setup

After cloning the repository, initialize the flake:

```bash
nix flake update
```

This will generate a `flake.lock` file that pins all dependencies.

## Building the Project

### Using Nix Flake

Build the default package (GHC 9.2.8):
```bash
nix build
```

Build with a specific GHC version:
```bash
nix build .#ghc-9-6    # GHC 9.6.6
nix build .#ghc-9-4    # GHC 9.4.8
nix build .#ghc-9-0    # GHC 9.0.2
nix build .#ghc-8-10   # GHC 8.10.7
```

### Development Shell

Enter the development environment (default GHC 9.2.8):
```bash
nix develop
```

Enter a development shell with a specific GHC version:
```bash
nix develop .#ghc-9-6
nix develop .#ghc-9-4
```

Inside the development shell, you can use familiar Cabal commands:
```bash
cabal build
cabal test
cabal repl
ghcid  # Auto-reload on file changes
```

### Using direnv (Recommended)

For automatic environment activation, install direnv and create a `.envrc` file:

```bash
echo "use flake" > .envrc
direnv allow
```

Now the development environment will activate automatically when you enter the directory.

## Running the Project

### Direct execution
```bash
nix run
```

### In development shell
```bash
nix develop
cabal run
```

## CI/CD Integration

The flake can be used in CI/CD pipelines:

```yaml
# Example GitHub Actions workflow
- name: Build with Nix
  run: nix build

- name: Test with Nix
  run: nix develop --command cabal test
```

## Differences from Stack

| Stack | Nix Flake |
|-------|-----------|
| `stack build` | `nix build` or `cabal build` (in dev shell) |
| `stack test` | `nix develop -c cabal test` |
| `stack repl` | `nix develop -c cabal repl` |
| `stack exec` | `nix run` |
| `stack.yaml` | `flake.nix` + `flake.lock` |

## Keeping Stack (Optional)

The `stack.yaml` file is still present and functional. You can use either:
- **Nix Flake** for reproducible builds and nixos integration
- **Stack** if you prefer or need stack-specific features

Both tools use the same `gothic.cabal` file as the source of truth.

## Updating Dependencies

### Update flake inputs
```bash
nix flake update
```

### Update Haskell dependencies
Modify `gothic.cabal` and rebuild:
```bash
nix build --recreate-lock-file
```

## Troubleshooting

### Build fails with "command not found"
Make sure you're in a nix develop shell or using `nix develop -c <command>`

### Flake is outdated
```bash
nix flake update
nix build
```

### Cache issues
```bash
nix build --rebuild
```

### Clean build
```bash
nix clean
nix build
```

## Benefits of Nix Flake

1. **Reproducibility**: Exact dependency versions locked in `flake.lock`
2. **Multi-version testing**: Easy to test against multiple GHC versions
3. **No system pollution**: Dependencies are isolated per-project
4. **Fast CI**: Nix binary cache speeds up CI builds significantly
5. **NixOS integration**: Native integration with NixOS systems

## Resources

- [Nix Flakes Documentation](https://nixos.wiki/wiki/Flakes)
- [Haskell on Nix](https://nixos.wiki/wiki/Haskell)
- [callCabal2nix](https://nixos.org/manual/nixpkgs/stable/#haskell-cabal2nix)
