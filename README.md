This is a CHIP-8 emulator written in Scala

Point it to a ROM and play away!  
The keyboard mapping is "documented" in `key_config` (sorry for the weird mapping).

Note to potential emu devs: This implementation diverges from usual implementations in the way it handles timers: Most implementations just let timers advance by one tick per instruction, this implementation uses the actual time that has passed since the timer was started.

Looking back, maybe it's not that good of an idea to write an emulator in a language in which the Byte type is signed.
