rsa-modul
=========

## INSTALL ##

* change to the project directory: `cd <project/path>`
* `ghc --make Main.hs -o rsa`

## Usage ##

* generate Keys: `./rsa keygen`. This command produces two files
  pubkey and privkey in the project directory.
  
* encrypt: `./rsa encrypt <pubkey> <file>`. Produce a file
  `<file>.encr`

* decrypt: `./rsa decrypt <privkey> <file>`. Print the decrypted
  String to stdout.
