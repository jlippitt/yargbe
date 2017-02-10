# YAR-GBE
Yet Another Rust Game Boy Emulator

## Current Features

* All graphics functionality working (barring bugs/glitches)
* All audio functionality working (barring bugs/glitches)
* Support for MBC1 and MBC3 cartridge types (though not for real time clock)
* 'Battery-backed RAM' (i.e. save files on disk)

## Features Still To Come

* Other cartridge types (and MBC3 real time clock)
* Game Boy Color support (maybe)

## What Probably Won't Be Implemented

* Serial port (as it doesn't seem to make much sense on an emulator)

## Usage

    yargbe [options] /path/to/my_rom.gb

The following options are supported:

| Option        |                                                                                              |
|---------------| ---------------------------------------------------------------------------------------------|
| -s, --scale N | Upscales the display by a factor of n in each direction. The default is 1 (i.e. no scaling). |
| -h, --help    | Displays usage information.                                                                  |

## Screenshots

![Super Mario Land](https://cloud.githubusercontent.com/assets/711298/14226476/551e191c-f8db-11e5-81a2-8609e71a5641.png)  ![The Legend of Zelda: Link's Awakening](https://cloud.githubusercontent.com/assets/711298/14226477/5a3faa46-f8db-11e5-99fa-6260cedc0078.png) ![Donkey Kong Land 2](https://cloud.githubusercontent.com/assets/711298/14226478/5d89e6da-f8db-11e5-81c0-f2adfbd05ce5.png)
