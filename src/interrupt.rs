use memory::Byte;
use std::fmt::{Display, Formatter, Result};

const INTERRUPTS: [Interrupt; 5] = [
    Interrupt::VBlank,
    Interrupt::LcdStat,
    Interrupt::Timer,
    Interrupt::Serial,
    Interrupt::Joypad
];

pub struct InterruptState {
    enabled: Byte,
    active: Byte
}

#[derive(Clone, Copy)]
pub enum Interrupt {
    VBlank = 0x01,
    LcdStat = 0x02,
    Timer = 0x04,
    Serial = 0x08,
    Joypad = 0x10
}

impl InterruptState {
    pub fn new() -> InterruptState {
        InterruptState {
            enabled: 0,
            active: 0
        }
    }

    pub fn enabled(&self) -> Byte {
        self.enabled
    }

    pub fn set_enabled(&mut self, value: Byte) {
        debug!("Interrupt mask: {:02X}", value);
        self.enabled = value;
    }

    pub fn active(&self) -> Byte {
        0xE0 | self.active
    }

    pub fn set_active(&mut self, value: Byte) {
        self.active = value;
    }

    pub fn fire(&mut self, interrupt: Interrupt) {
        self.active |= interrupt as Byte;
    }

    pub fn reset(&mut self, interrupt: Interrupt) {
        self.active &= !(interrupt as Byte);
    }

    pub fn has_interrupt(&self, interrupt: Interrupt) -> bool {
        (interrupt as Byte) & self.enabled & self.active != 0
    }

    pub fn next_interrupt(&self) -> Option<Interrupt> {
        for &interrupt in INTERRUPTS.iter() {
            let byte = interrupt as Byte;

            if byte & self.enabled & self.active != 0 {
                return Some(interrupt)
            }
        }

        None
    }
}

impl Display for Interrupt {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", match *self {
            Interrupt::VBlank => "VBlank",
            Interrupt::LcdStat => "LCD Stat",
            Interrupt::Timer => "Timer Overflow",
            Interrupt::Serial => "Serial Transfer Complete",
            Interrupt::Joypad => "Joypad"
        })
    }
}
