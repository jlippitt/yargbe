use memory::Byte;
use memmap::{Mmap, Protection};
use std::fmt::{Display, Formatter, Result};
use std::fs::{File, OpenOptions};
use std::io::Read;
use std::ops::{Index, IndexMut};
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
    ram: Ram,
    ram_enabled: bool,
    ram_bank: Byte,
    ram_offset: usize
}

#[derive(Default)]
struct CartridgeType {
    mapper: Mapper,
    timer_enabled: bool, // Note: Not currently used
    ram_enabled: bool,
    save_enabled: bool
}

enum Mapper {
    RomOnly,
    Mbc1,
    Mbc3
}

enum Mode {
    Rom,
    Ram
}

type Ram = Box<IndexMut<usize, Output=Byte>>;

struct SaveFile {
    data: Mmap
}

impl Rom {
    pub fn new(path: &Path) -> Rom {
        let mut data = Vec::new();

        File::open(path)
            .unwrap()
            .read_to_end(&mut data)
            .unwrap();

        let cartridge_type = CartridgeType::from(data[0x0147]);

        info!("Cartridge Type: {}", cartridge_type);

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

        info!("ROM size: {} ({} banks)", data.len(), rom_bank_count);

        let ram_size: usize = match data[0x0149] {
            0x00 => 0,
            0x01 => 2048,
            0x02 => 8192,
            0x03 => 32768,
            0x04 => 131072,
            value @ _ => panic!("Invalid RAM size byte: {:02X}", value)
        };

        info!("RAM size: {}", ram_size);

        let ram: Ram = if cartridge_type.save_enabled {
            Box::new(SaveFile::new(path, ram_size))
        } else {
            Box::new(vec![0x00; ram_size])
        };

        Rom {
            data: data,
            cartridge_type: cartridge_type,
            mode: Mode::Rom,
            rom_bank: 0,
            rom_bank_mask: rom_bank_count - 1,
            rom_offset: ROM_BANK_SIZE,
            ram: ram,
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
                if self.cartridge_type.ram_enabled {
                    self.ram_enabled = (value & 0x0A) != 0;
                }
            },
            0x2000 ... 0x3FFF => {
                match self.cartridge_type.mapper {
                    Mapper::Mbc1 => {
                        let mut low_bank = value & 0x1F;

                        // ROM bank $00 acts the same as $01, $20 as $21, etc.
                        if low_bank == 0x00 {
                            low_bank = 0x01;
                        }

                        let rom_bank = (self.rom_bank & 0x60) + low_bank;
                        self.set_rom_bank(rom_bank);
                    },
                    Mapper::Mbc3 => {
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
                match self.cartridge_type.mapper {
                    Mapper::Mbc1 => {
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
                    Mapper::Mbc3 => {
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
                match self.cartridge_type.mapper {
                    Mapper::Mbc1 => {
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
                    Mapper::Mbc3 => {
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

impl From<Byte> for CartridgeType {
    fn from(value: Byte) -> CartridgeType {
        match value {
            0x00 => CartridgeType {
                mapper: Mapper::RomOnly,
                ..Default::default()
            },
            0x01 => CartridgeType {
                mapper: Mapper::Mbc1,
                ..Default::default()
            },
            0x02 => CartridgeType {
                mapper: Mapper::Mbc1,
                ram_enabled: true,
                ..Default::default()
            },
            0x03 => CartridgeType {
                mapper: Mapper::Mbc1,
                ram_enabled: true,
                save_enabled: true,
                ..Default::default()
            },
            0x0F => CartridgeType {
                mapper: Mapper::Mbc3,
                timer_enabled: true,
                save_enabled: true,
                ..Default::default()
            },
            0x10 => CartridgeType {
                mapper: Mapper::Mbc3,
                timer_enabled: true,
                ram_enabled: true,
                save_enabled: true,
                ..Default::default()
            },
            0x11 => CartridgeType {
                mapper: Mapper::Mbc3,
                ..Default::default()
            },
            0x12 => CartridgeType {
                mapper: Mapper::Mbc3,
                ram_enabled: true,
                ..Default::default()
            },
            0x13 => CartridgeType {
                mapper: Mapper::Mbc3,
                ram_enabled: true,
                save_enabled: true,
                ..Default::default()
            },
            _ => panic!("Cartridge type ${:02X} not yet supported", value)
        }
    }
}

impl Display for CartridgeType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        try!(write!(f, "{}", self.mapper));

        if self.timer_enabled {
            try!(write!(f, " + TIMER"));
        }

        if self.ram_enabled {
            try!(write!(f, " + RAM"));
        }

        if self.save_enabled {
            try!(write!(f, " + BATT"));
        }

        Ok(())
    }
}

impl Default for Mapper {
    fn default() -> Mapper {
        Mapper::RomOnly
    }
}

impl Display for Mapper {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", match *self {
            Mapper::RomOnly => "ROM Only",
            Mapper::Mbc1 => "MBC1",
            Mapper::Mbc3 => "MBC3"
        })
    }
}

impl SaveFile {
    fn new(path: &Path, size: usize) -> SaveFile {
        let save_path = path.with_extension("sav");

        let save_file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(save_path)
            .unwrap();

        save_file.set_len(size as u64).unwrap();

        let data = Mmap::open_with_offset(&save_file, Protection::ReadWrite, 0, size).unwrap();
        
        SaveFile {
            data: data
        }
    }
}

impl Index<usize> for SaveFile {
    type Output = Byte;

    fn index<'a>(&'a self, offset: usize) -> &'a Byte {
        let data = unsafe { self.data.as_slice() };
        &data[offset]
    }
}

impl IndexMut<usize> for SaveFile {
    fn index_mut<'a>(&'a mut self, offset: usize) -> &'a mut Byte {
        let data = unsafe { self.data.as_mut_slice() };
        &mut data[offset]
    }
}
