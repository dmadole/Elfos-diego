# Diego Storage Driver

This is a loadable storage driver for Elf/OS supporting an SD Card on a very minimal hardware interface connected only to Q and EF2. This supports SDHC and SDXC cards at this time and can transfer data at about 5K per second.

Right now, this replaces the current disk device with the SD Card when loaded. There are no tools available at this time to create a usable SD Card. The simplest method is create one currently is to use a PC to copy an image of a working CF Card onto an SD Card. I used Win32 Disk Imager to do this, but other tools including dd on Linux would be suitable as well.

Currently this reads and writes SD, HC, and XC cards. Hot-swap is supported by the driver, meaning it will detect the card is changed and initialize the new card and read it's capacity. However, any currently loaded sectors in system file descriptors are not yet invalidated.

This is still a little experimental, but it is now pretty close to what I would consider a clean release. I welcome any input or collaboration.

P.S. Why is it called Diego? Because "SD" always reminds me of the city San Diego and I have traditionally named device drivers names ending in "o".
