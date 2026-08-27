# Gothic, a Haskell client for HashiCorp Vault KV Engine v2 ![CI](https://github.com/MichelBoucey/gothic/actions/workflows/haskell-ci.yml/badge.svg) [![Hackage](https://img.shields.io/hackage/v/gothic.svg)](https://hackage.haskell.org/package/gothic)

> "Historically, strongrooms were built in the basement of a bank where the ceilings were vaulted, hence the name." Art.
"Bank vault", Wikipedia.

## 1. Goal

This library implements the [HashiCorp Vault KVv2 Engine API](https://www.vaultproject.io/api/secret/kv/kv-v2.html).

## 2. Test suite

`cabal test` needs an `Hashicorp Vault` docker container started with:

`docker run --cap-add=IPC_LOCK -e 'VAULT_DEV_ROOT_TOKEN_ID=A_SECRET_TOKEN' -d --name=dev-vault -p 8200:8200 hashicorp/vault`

Latest successfully tested `HashiCorp Vault` version: `2.0.4`.

## 3. License

Since `v0.2.0.0`, `Gothic` is released under `GPLv3`.
