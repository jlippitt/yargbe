use memory::Byte;
use std::fmt::{Display, Formatter, Result};
use std::fs::File;
use std::io::Read;
use std::path::Path;

const ROM_BANK_SIZE: usize = 0x4000;
const RAM_BANK_SIZE: usize = 0x2000;

pub struct Rom {
    data: Vec<Byte>,
    cartridge_type: CartridgeType,
    mode: Mode,
    rom_bank: Byte,
    rom_bank_mask: Byte,
    rom_offset: usize,
    ram: Vec<Byte>,
    ram_enabled: bool,
    ram_bank: Byte,
    ram_offset: usize
}

enum CartridgeType {
    RomOnly,
    Mbc1,
    Mbc3
}

enum Mode {
    Rom,
    Ram
}

impl Rom {
    pub fn new(path: &Path) -> Rom {
        let mut data = Vec::new();

        File::open(path)
            .unwrap()
            .read_to_end(&mut data)
            .unwrap();

        let cartridge_type = match data[0x0147] {
            0x00 => CartridgeType::RomOnly,
            0x01 ... 0x03 => CartridgeType::Mbc1,
            0x0F ... 0x13 => CartridgeType::Mbc3,
            value @ _ => panic!("Unsupported cartridge type: {:02X}", value)
        };

        debug!("Cartridge Type: {}", cartridge_type);

        let rom_bank_count = match data[0x0148] {
            0x00 => 2,
            0x01 => 4,
            0x02 => 8,
            0x03 => 16,
            0x04 => 32,
            0x05 => 64,
            0x06 => 128,
            0x52 => 72,
            0x53 => 80,
            0x54 => 96,
            value @ _ => panic!("Unable to determine number of ROM banks: {:02X}", value)
        };

        debug!("ROM size: {} ({} banks)", data.len(), rom_bank_count);

        let ram_size: usize = match data[0x0149] {
            0x00 => 0,
            0x01 => 2048,
            0x02 => 8192,
            0x03 => 32768,
            0x04 => 131072,
            value @ _ => panic!("Invalid RAM size byte: {:02X}", value)
        };

        debug!("RAM size: {}", ram_size);

        Rom {
            data: data,
            cartridge_type: cartridge_type,
            mode: Mode::Rom,
            rom_bank: 0,
            rom_bank_mask: rom_bank_count - 1,
            rom_offset: ROM_BANK_SIZE,
            ram: vec![0x00; ram_size],
            ram_enabled: false,
            ram_bank: 0,
            ram_offset: 0
        }
    }

    pub fn rom_byte(&self, offset: usize) -> Byte {
        match offset {
            0x0000 ... 0x3FFF => {
                self.data[offset]
            },
            0x4000 ... 0x7FFF => {
                self.data[self.rom_offset + (offset & 0x3FFF)]
            }
            _ => panic!("Read from unsupported area of ROM: {:04X}", offset)
        }
    }

    pub fn put_rom_byte(&mut self, offset: usize, value: Byte) {
        debug!("MBC control: {:04X} = {:02X}", offset, value);

        match offset {
            0x0000 ... 0x1FFF => {
                // Enable/disable RAM (if there is any)
                if self.ram.len() > 0 {
                    self.ram_enabled = (value & 0x0A) != 0;
                }
            },
            0x2000 ... 0x3FFF => {
                match self.cartridge_type {
                    CartridgeType::Mbc1 => {
                        let mut low_bank = value & 0x1F;

                        // ROM bank $00 acts the same as $01, $20 as $21, etc.
                        if low_bank == 0x00 {
                            low_bank = 0x01;
                        }

                        let rom_bank = (self.rom_bank & 0x60) + low_bank;
                        self.set_rom_bank(rom_bank);
                    },
                    CartridgeType::Mbc3 => {
                        let mut rom_bank = value & 0x7F;

                        // ROM bank $00 acts the same as $01 (but $20 is $20)
                        if rom_bank == 0x00 {
                            rom_bank = 0x01;
                        }

                        self.set_rom_bank(rom_bank);
                    },
                    _ => debug!("ROM bank selections not enabled for this cartridge type")
                }
            },
            0x4000 ... 0x5FFF => {
                match self.cartridge_type {
                    CartridgeType::Mbc1 => {
                        match self.mode {
                            Mode::Rom => {
                                let rom_bank = ((value << 5) & 0x60) + (self.rom_bank & 0x1F);
                                self.set_rom_bank(rom_bank);
                            },
                            Mode::Ram => {
                                self.set_ram_bank(value & 0x03);
                            }
                        }
                    },
                    CartridgeType::Mbc3 => {
                        if value <= 0x03 {
                            // RAM bank select
                            self.set_ram_bank(value);
                        } else {
                            // TODO: Clock
                        }
                    },
                    _ => debug!("ROM bank selections not enabled for this cartridge type")
                }
            },
            0x6000 ... 0x7FFF => {
                match self.cartridge_type {
                    CartridgeType::Mbc1 => {
                        self.mode = match value & 0x01 {
                            0x00 => Mode::Rom,
                            0x01 => {
                                // Clear the upper two bits of the ROM bank when entering RAM mode
                                let rom_bank = self.rom_bank & 0x1F;
                                self.set_rom_bank(rom_bank);
                                Mode::Ram
                            },
                            _ => unreachable!()
                        }
                    },
                    CartridgeType::Mbc3 => {
                        // TODO: Clock
                    },
                    _ => debug!("Mode switch not enabled for this cartridge type")
                }
            },
            _ => unreachable!()
        };
    }

    pub fn ram_byte(&self, offset: usize) -> Byte {
        if self.ram_enabled {
            // In ROM mode, we can only access bank 0
            let offset = match self.mode {
                Mode::Ram => self.ram_offset + offset,
                Mode::Rom => offset
            };

            self.ram[offset]

        } else {
            // Always returns this value
            0xFF
        }
    }

    pub fn put_ram_byte(&mut self, offset: usize, value: Byte) {
        if self.ram_enabled {
            // In ROM mode, we can only access bank 0
            let offset = match self.mode {
                Mode::Ram => self.ram_offset + offset,
                Mode::Rom => offset
            };

            self.ram[offset] = value;
        }
    }

    fn set_rom_bank(&mut self, rom_bank: Byte) {
        self.rom_bank = rom_bank & self.rom_bank_mask;
        self.rom_offset = ROM_BANK_SIZE * self.rom_bank as usize;
        debug!("ROM bank {:02X} selected; offset = {:04X}", self.rom_bank, self.rom_offset);
    }

    fn set_ram_bank(&mut self, ram_bank: Byte) {
        self.ram_bank = ram_bank;
        self.ram_offset = RAM_BANK_SIZE * self.ram_bank as usize;
        debug!("RAM bank {:02X} selected; offset = {:04X}", self.ram_bank, self.ram_offset);
    }
}

impl Display for CartridgeType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", match *self {
            CartridgeType::RomOnly => "ROM Only",
            CartridgeType::Mbc1 => "MBC1",
            CartridgeType::Mbc3 => "MBC3"
        })
    }
}
