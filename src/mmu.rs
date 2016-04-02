use audio::AudioState;
use gpu::{GpuState, OAM_SIZE};
use interrupt::InterruptState;
use joypad::JoypadState;
use memory::{Byte, Word};
use rom::Rom;
use std::cell::RefCell;
use std::rc::Rc;
use timer::TimerState;

const BIOS: [Byte; 256] = [
    0x31, 0xFE, 0xFF, 0xAF, 0x21, 0xFF, 0x9F, 0x32, 0xCB, 0x7C, 0x20, 0xFB, 0x21, 0x26, 0xFF, 0x0E,
    0x11, 0x3E, 0x80, 0x32, 0xE2, 0x0C, 0x3E, 0xF3, 0xE2, 0x32, 0x3E, 0x77, 0x77, 0x3E, 0xFC, 0xE0,
    0x47, 0x11, 0x04, 0x01, 0x21, 0x10, 0x80, 0x1A, 0xCD, 0x95, 0x00, 0xCD, 0x96, 0x00, 0x13, 0x7B,
    0xFE, 0x34, 0x20, 0xF3, 0x11, 0xD8, 0x00, 0x06, 0x08, 0x1A, 0x13, 0x22, 0x23, 0x05, 0x20, 0xF9,
    0x3E, 0x19, 0xEA, 0x10, 0x99, 0x21, 0x2F, 0x99, 0x0E, 0x0C, 0x3D, 0x28, 0x08, 0x32, 0x0D, 0x20,
    0xF9, 0x2E, 0x0F, 0x18, 0xF3, 0x67, 0x3E, 0x64, 0x57, 0xE0, 0x42, 0x3E, 0x91, 0xE0, 0x40, 0x04,
    0x1E, 0x02, 0x0E, 0x0C, 0xF0, 0x44, 0xFE, 0x90, 0x20, 0xFA, 0x0D, 0x20, 0xF7, 0x1D, 0x20, 0xF2,
    0x0E, 0x13, 0x24, 0x7C, 0x1E, 0x83, 0xFE, 0x62, 0x28, 0x06, 0x1E, 0xC1, 0xFE, 0x64, 0x20, 0x06,
    0x7B, 0xE2, 0x0C, 0x3E, 0x87, 0xF2, 0xF0, 0x42, 0x90, 0xE0, 0x42, 0x15, 0x20, 0xD2, 0x05, 0x20,
    0x4F, 0x16, 0x20, 0x18, 0xCB, 0x4F, 0x06, 0x04, 0xC5, 0xCB, 0x11, 0x17, 0xC1, 0xCB, 0x11, 0x17,
    0x05, 0x20, 0xF5, 0x22, 0x23, 0x22, 0x23, 0xC9, 0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B,
    0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D, 0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E,
    0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99, 0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC,
    0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E, 0x3c, 0x42, 0xB9, 0xA5, 0xB9, 0xA5, 0x42, 0x4C,
    0x21, 0x04, 0x01, 0x11, 0xA8, 0x00, 0x1A, 0x13, 0xBE, 0x20, 0xFE, 0x23, 0x7D, 0xFE, 0x34, 0x20,
    0xF5, 0x06, 0x19, 0x78, 0x86, 0x23, 0x05, 0x20, 0xFB, 0x86, 0x20, 0xFE, 0x3E, 0x01, 0xE0, 0x50
];

const WRAM_SIZE: usize = 8192;
const ZRAM_SIZE: usize = 127;

pub struct Mmu {
    rom: Rom,
    audio_state: Rc<RefCell<AudioState>>,
    gpu_state: Rc<RefCell<GpuState>>,
    timer_state: Rc<RefCell<TimerState>>,
    joypad_state: Rc<RefCell<JoypadState>>,
    interrupt_state: Rc<RefCell<InterruptState>>,
    wram: [Byte; WRAM_SIZE],
    zram: [Byte; ZRAM_SIZE],
    bios_flag: bool
}

impl Mmu {
    pub fn new(
        rom: Rom,
        audio_state: Rc<RefCell<AudioState>>,
        gpu_state: Rc<RefCell<GpuState>>,
        timer_state: Rc<RefCell<TimerState>>,
        joypad_state: Rc<RefCell<JoypadState>>,
        interrupt_state: Rc<RefCell<InterruptState>>
    ) -> Mmu {
        Mmu {
            rom: rom,
            audio_state: audio_state,
            gpu_state: gpu_state,
            timer_state: timer_state,
            joypad_state: joypad_state,
            interrupt_state: interrupt_state,
            wram: [0; 8192],
            zram: [0; 127],
            bios_flag: true
        }
    }

    pub fn byte(&self, address: Word) -> Byte {
        let address = address.get();

        let result = match address {
            0x0000 ... 0x00FF => {
                // BIOS or ROM (depending on whether the BIOS has finished
                // running yet)
                if self.bios_flag {
                    BIOS[address as usize]
                } else {
                    self.rom.rom_byte(address as usize)
                }
            },
            0x0100 ... 0x7FFF => {
                // ROM
                self.rom.rom_byte(address as usize)
            },
            0x8000 ... 0x9FFF => {
                // VRAM
                self.gpu_state.borrow().vram_byte((address & 0x1FFF) as usize)
            },
            0xA000 ... 0xBFFF => {
                // External RAM
                self.rom.ram_byte((address & 0x1FFF) as usize)
            },
            0xC000 ... 0xFDFF => {
                // Working RAM
                self.wram[(address & 0x1FFF) as usize]
            },
            0xFE00 ... 0xFE9F => {
                // Object Attribute Memory
                self.gpu_state.borrow().oam_byte((address & 0x00FF) as usize)
            },
            0xFEA0 ... 0xFEFF => {
                // Unused space after OAM
                0
            },
            0xFF00 ... 0xFFFF => {
                self.high_byte(address as Byte)
            },
            _ => panic!("This shouldn't happen: {:04X}", address)
        };

        trace!("Read: {:04X} => {:02X}", address, result);

        result
    }

    pub fn high_byte(&self, address: Byte) -> Byte {
        let result = match address {
            0x00 => {
                // Joypad
                self.joypad_state.borrow().byte()
            },
            0x04 ... 0x07 => {
                // Timer registers
                self.timer_state.borrow().byte(address as usize)
            },
            0x0F => {
                // Interrupt set flag
                self.interrupt_state.borrow().active()
            },
            0x10 ... 0x3F => {
                // Audio registers
                self.audio_state.borrow().byte(address as usize)
            },
            0x40 ... 0x45 | 0x47 ... 0x4B => {
                // GPU registers
                self.gpu_state.borrow()
                    .register_byte((address & 0x0F) as usize)
            },
            0x46 => {
                // DMA transfer start address. Can't read from this.
                0
            },
            0x4D => {
                // Double speed mode register for GBC. On DMG, always returns $FF.
                0xFF
            },
            0x50 => {
                // BIOS flag can't be read
                0
            },
            0x80 ... 0xFE => {
                // Zero Page RAM
                self.zram[(address & 0x7F) as usize]
            },
            0xFF => {
                // Interrupt enable/disable
                self.interrupt_state.borrow().enabled()
            },
            _ => {
                warn!("Read from unimplemented register: {:4X}", address);
                0
            }
        };

        trace!("Read (High): {:02X} => {:02X}", address, result);

        result
    }

    pub fn word(&self, address: Word) -> Word {
        Word::from_bytes(self.byte(address + 1), self.byte(address))
    }

    pub fn put_byte(&mut self, address: Word, value: Byte) {
        let address = address.get();

        match address {
            0x0000 ... 0x7FFF => {
                // ROM
                self.rom.put_rom_byte(address as usize, value);
            },
            0x8000 ... 0x9FFF => {
                // VRAM
                self.gpu_state.borrow_mut()
                    .put_vram_byte((address & 0x1FFF) as usize, value);
            },
            0xA000 ... 0xBFFF => {
                // External RAM
                self.rom.put_ram_byte((address & 0x1FFF) as usize, value);
            },
            0xC000 ... 0xFDFF => {
                // Working RAM
                self.wram[(address & 0x1FFF) as usize] = value;
            },
            0xFE00 ... 0xFE9F => {
                // Object Attribute Memory
                self.gpu_state.borrow_mut()
                    .put_oam_byte((address & 0x00FF) as usize, value);
            },
            0xFEA0 ... 0xFEFF => {
                // Unused space after OAM
            },
            0xFF00 ... 0xFFFF => {
                self.put_high_byte(address as Byte, value);
            },
            _ => panic!("This shouldn't happen: {:04X}", address)
        };

        trace!("Write: {:04X} <= {:02X}", address, value);
    }

    pub fn put_high_byte(&mut self, address: Byte, value: Byte) {
        match address {
            0x00 => {
                // Joypad
                self.joypad_state.borrow_mut().put_byte(value);
            },
            0x01 => {
                // TODO: Serial transfer (eventually)
                // Nothing yet
            },
            0x02 => {
                // TODO: Serial transfer (eventually)
                // Nothing yet
            }
            0x04 ... 0x07 => {
                // Timer registers
                self.timer_state.borrow_mut()
                    .put_byte(address as usize, value);
            },
            0x0F => {
                // Interrupt set flag
                self.interrupt_state.borrow_mut().set_active(value);
            },
            0x10 ... 0x3F => {
                // Audio registers
                self.audio_state.borrow_mut().put_byte(address as usize, value);
            },
            0x40 ... 0x45 | 0x47 ... 0x4B => {
                // GPU registers
                self.gpu_state.borrow_mut()
                    .put_register_byte((address & 0x0F) as usize, value);
            },
            0x46 => {
                // Start a DMA transfer
                self.dma_transfer(Word::from_bytes(value, 0));
            },
            0x4D => {
                // Request double speed mode. Unsupported by DMG (only GBC).
            },
            0x50 => {
                // A write to here un-loads the BIOS
                self.bios_flag = false;
            },
            0x80 ... 0xFE => {
                // Zero Page RAM
                self.zram[(address & 0x7F) as usize] = value;
            },
            0xFF => {
                // Interrupt enable/disable
                self.interrupt_state.borrow_mut().set_enabled(value);
            },
            _ => {
                warn!("Write to unimplemented register: {:4X}", address);
            }
        };

        trace!("Write (High): {:02X} <= {:02X}", address, value);
    }

    pub fn put_word(&mut self, address: Word, value: Word) {
        self.put_byte(address, value.low);
        self.put_byte(address + 1, value.high);
    }

    fn dma_transfer(&mut self, start_address: Word) {
        for i in 0..OAM_SIZE {
            let value = self.byte(start_address + i as u16);
            self.gpu_state.borrow_mut().put_oam_byte(i, value);
        }
    }
}
