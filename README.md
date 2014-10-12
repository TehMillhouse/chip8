This is a CHIP-8 emulator written in Scala

Point it to a ROM and play away!  
The keyboard mapping is "documented" in `key_config` (sorry for the weird mapping).

Looking back, maybe it's not that good of an idea to write an emulator in a language in which the Byte type is signed.

Known issues:
-------------

* Keyboard input is still somewhat weird in some games
* Collisions seem off (as seen in PONG)
* No sound support
