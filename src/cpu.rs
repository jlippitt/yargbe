use interrupt::{Interrupt, InterruptState};
use memory::{Byte, Word};
use mmu::Mmu;
use std::cell::RefCell;
use std::fmt::{Display, Formatter, Result};
use std::rc::Rc;

pub const CYCLES_PER_SECOND: u64 = 1048576;

const ZERO: Byte = 0x80;
const SUBTRACT: Byte = 0x40;
const HALF_CARRY: Byte = 0x20;
const CARRY: Byte = 0x10;

const OP_TIMES: [u64; 256] = [
    1, 3, 2, 2, 1, 1, 2, 1, 5, 2, 2, 2, 1, 1, 2, 1, 
    0, 3, 2, 2, 1, 1, 2, 1, 3, 2, 2, 2, 1, 1, 2, 1, 
    2, 3, 2, 2, 1, 1, 2, 1, 2, 2, 2, 2, 1, 1, 2, 1, 
    2, 3, 2, 2, 3, 3, 3, 1, 2, 2, 2, 2, 1, 1, 2, 1, 
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 
    2, 2, 2, 2, 2, 2, 0, 2, 1, 1, 1, 1, 1, 1, 2, 1, 
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 
    1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 
    2, 3, 3, 4, 3, 4, 2, 4, 2, 4, 3, 0, 3, 6, 2, 4, 
    2, 3, 3, 0, 3, 4, 2, 4, 2, 4, 3, 0, 3, 0, 2, 4, 
    3, 3, 2, 0, 0, 4, 2, 4, 4, 1, 4, 0, 0, 0, 2, 4, 
    3, 3, 2, 1, 0, 4, 2, 4, 3, 2, 4, 1, 0, 0, 2, 4
];

const EXTENDED_OP_TIMES: [u64; 256] = [
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 
    2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, 
    2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, 
    2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, 
    2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, 
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, 
    2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2
];

pub struct Cpu {
    mmu: Mmu,
    interrupt_state: Rc<RefCell<InterruptState>>,
    clock: u64,
    halted: bool,
    stopped: bool,
    ime: bool,
    ime_delayed: bool,
    af: Word,
    bc: Word,
    de: Word,
    hl: Word,
    sp: Word,
    pc: Word
}

enum FlagCondition {
    NotZero,
    Zero,
    NotCarry,
    Carry
}

trait Reader<T> : Display {
    fn read(&self, cpu: &mut Cpu) -> T;
}

trait Writer<T> : Reader<T> {
    fn write(&self, cpu: &mut Cpu, value: T);
}

#[macro_export]
macro_rules! register_byte_accessor {
    ($class:ident, $name:expr, $reg:ident, $byte:ident) => {
        struct $class;

        impl Reader<Byte> for $class {
            fn read(&self, cpu: &mut Cpu) -> Byte {
                cpu.$reg.$byte
            }
        }

        impl Writer<Byte> for $class {
            fn write(&self, cpu: &mut Cpu, value: Byte) {
                cpu.$reg.$byte = value;
            }
        }

        impl ::std::fmt::Display for $class {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                write!(f, $name)
            }
        }
    }
}

#[macro_export]
macro_rules! register_word_accessor {
    ($class:ident, $name:expr, $reg:ident) => {
        struct $class;

        impl Reader<Word> for $class {
            fn read(&self, cpu: &mut Cpu) -> Word {
                cpu.$reg
            }
        }

        impl Writer<Word> for $class {
            fn write(&self, cpu: &mut Cpu, value: Word) {
                cpu.$reg = value;
            }
        }

        impl ::std::fmt::Display for $class {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                write!(f, $name)
            }
        }
    }
}

#[macro_export]
macro_rules! memory_byte_accessor {
    ($class:ident, $name:expr, $reg:ident) => {
        struct $class;

        impl Reader<Byte> for $class {
            fn read(&self, cpu: &mut Cpu) -> Byte {
                cpu.mmu.byte(cpu.$reg)
            }
        }

        impl Writer<Byte> for $class {
            fn write(&self, cpu: &mut Cpu, value: Byte) {
                cpu.mmu.put_byte(cpu.$reg, value);
            }
        }

        impl ::std::fmt::Display for $class {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                write!(f, "({})", $name)
            }
        }
    }
}

register_byte_accessor!(RegA, "A", af, high);
register_byte_accessor!(RegB, "B", bc, high);
register_byte_accessor!(RegC, "C", bc, low);
register_byte_accessor!(RegD, "D", de, high);
register_byte_accessor!(RegE, "E", de, low);
register_byte_accessor!(RegH, "H", hl, high);
register_byte_accessor!(RegL, "L", hl, low);

register_word_accessor!(RegAF, "AF", af);
register_word_accessor!(RegBC, "BC", bc);
register_word_accessor!(RegDE, "DE", de);
register_word_accessor!(RegHL, "HL", hl);
register_word_accessor!(RegSP, "SP", sp);

memory_byte_accessor!(MemBC, "BC", bc);
memory_byte_accessor!(MemDE, "DE", de);
memory_byte_accessor!(MemHL, "HL", hl);

struct Immediate;
struct Immediate16;
struct MemImmediate;
struct MemImmediate16;
struct MemCHigh;
struct MemImmediateHigh;

impl Cpu {
    pub fn new(mmu: Mmu, interrupt_state: Rc<RefCell<InterruptState>>) -> Cpu {
        Cpu {
            mmu: mmu,
            interrupt_state: interrupt_state,
            clock: 0,
            halted: false,
            stopped: false,
            ime: false,
            ime_delayed: false,
            af: Word::new(0x0000),
            bc: Word::new(0x0000),
            de: Word::new(0x0000),
            hl: Word::new(0x0000),
            sp: Word::new(0x0000),
            pc: Word::new(0x0000)
        }
    }

    pub fn step(&mut self) -> u64 {
        // When stopped, we do nothing until a button is pressed. We don't even
        // increment the clock!
        if self.stopped {
            if self.interrupt_state.borrow().has_interrupt(Interrupt::Joypad) {
                self.stopped = false;
            } else {
                return 0;
            }
        }

        let clock_begin = self.clock;

        // Handle the one instruction delay of the EI instruction
        if self.ime_delayed {
            self.ime = true;
            self.ime_delayed = false;
        }

        if self.halted {
            // Just increment the clock by one tick
            self.clock += 1;

        } else {
            // Interpret the next instruction
            self.op();

            trace!(
                "State: AF={:04X} BC={:04X} DE={:04X} HL={:04X} SP={:04X} PC={:04X} M={}",
                self.af.get(),
                self.bc.get(),
                self.de.get(),
                self.hl.get(),
                self.sp.get(),
                self.pc.get(),
                self.clock
            );
        }

        let maybe_interrupt = self.interrupt_state.borrow().next_interrupt();

        if let Some(interrupt) = maybe_interrupt {
            // When halted, un-halt the CPU regardless of whether IME is enabled
            if self.halted {
                self.halted = false;
                self.clock += 1;
            }

            // When IME is enabled, execute the interrupt routine
            if self.ime {
                debug!("Interrupt: {}", interrupt);

                // Reset corresponding flag in the IF register
                self.interrupt_state.borrow_mut().reset(interrupt);

                // Prevent further interrupts
                self.ime = false;

                // Perform a RST to the interrupt address
                self.rst(match interrupt {
                    Interrupt::VBlank => 0x40,
                    Interrupt::LcdStat => 0x48,
                    Interrupt::Timer => 0x50,
                    Interrupt::Serial => 0x58,
                    Interrupt::Joypad => 0x60
                });

                self.clock += 5;
            }
        }

        // Return the amount of time taken to process this instruction
        self.clock - clock_begin
    }

    fn op(&mut self) {
        let op = self.next_byte();

        match op {
            0x00 => self.nop(),
            0x01 => self.ld16(&RegBC, &Immediate16),
            0x02 => self.ld(&MemBC, &RegA),
            0x03 => self.inc16(&RegBC),
            0x04 => self.inc(&RegB),
            0x05 => self.dec(&RegB),
            0x06 => self.ld(&RegB, &Immediate),
            0x07 => self.rlca(),
            0x08 => self.ld16(&MemImmediate16, &RegSP),
            0x09 => self.add16(&RegBC),
            0x0A => self.ld(&RegA, &MemBC),
            0x0B => self.dec16(&RegBC),
            0x0C => self.inc(&RegC),
            0x0D => self.dec(&RegC),
            0x0E => self.ld(&RegC, &Immediate),
            0x0F => self.rrca(),
            0x10 => self.stop(),
            0x11 => self.ld16(&RegDE, &Immediate16),
            0x12 => self.ld(&MemDE, &RegA),
            0x13 => self.inc16(&RegDE),
            0x14 => self.inc(&RegD),
            0x15 => self.dec(&RegD),
            0x16 => self.ld(&RegD, &Immediate),
            0x17 => self.rla(),
            0x18 => self.jr(),
            0x19 => self.add16(&RegDE),
            0x1A => self.ld(&RegA, &MemDE),
            0x1B => self.dec16(&RegDE),
            0x1C => self.inc(&RegE),
            0x1D => self.dec(&RegE),
            0x1E => self.ld(&RegE, &Immediate),
            0x1F => self.rra(),
            0x20 => self.jr_cc(FlagCondition::NotZero),
            0x21 => self.ld16(&RegHL, &Immediate16),
            0x22 => self.ldi(&MemHL, &RegA),
            0x23 => self.inc16(&RegHL),
            0x24 => self.inc(&RegH),
            0x25 => self.dec(&RegH),
            0x26 => self.ld(&RegH, &Immediate),
            0x27 => self.daa(),
            0x28 => self.jr_cc(FlagCondition::Zero),
            0x29 => self.add16(&RegHL),
            0x2A => self.ldi(&RegA, &MemHL),
            0x2B => self.dec16(&RegHL),
            0x2C => self.inc(&RegL),
            0x2D => self.dec(&RegL),
            0x2E => self.ld(&RegL, &Immediate),
            0x2F => self.cpl(),
            0x30 => self.jr_cc(FlagCondition::NotCarry),
            0x31 => self.ld16(&RegSP, &Immediate16),
            0x32 => self.ldd(&MemHL, &RegA),
            0x33 => self.inc16(&RegSP),
            0x34 => self.inc(&MemHL),
            0x35 => self.dec(&MemHL),
            0x36 => self.ld(&MemHL, &Immediate),
            0x37 => self.scf(),
            0x38 => self.jr_cc(FlagCondition::Carry),
            0x39 => self.add16(&RegSP),
            0x3A => self.ldd(&RegA, &MemHL),
            0x3B => self.dec16(&RegSP),
            0x3C => self.inc(&RegA),
            0x3D => self.dec(&RegA),
            0x3E => self.ld(&RegA, &Immediate),
            0x3F => self.ccf(),
            0x40 => self.ld(&RegB, &RegB),
            0x41 => self.ld(&RegB, &RegC),
            0x42 => self.ld(&RegB, &RegD),
            0x43 => self.ld(&RegB, &RegE),
            0x44 => self.ld(&RegB, &RegH),
            0x45 => self.ld(&RegB, &RegL),
            0x46 => self.ld(&RegB, &MemHL),
            0x47 => self.ld(&RegB, &RegA),
            0x48 => self.ld(&RegC, &RegB),
            0x49 => self.ld(&RegC, &RegC),
            0x4A => self.ld(&RegC, &RegD),
            0x4B => self.ld(&RegC, &RegE),
            0x4C => self.ld(&RegC, &RegH),
            0x4D => self.ld(&RegC, &RegL),
            0x4E => self.ld(&RegC, &MemHL),
            0x4F => self.ld(&RegC, &RegA),
            0x50 => self.ld(&RegD, &RegB),
            0x51 => self.ld(&RegD, &RegC),
            0x52 => self.ld(&RegD, &RegD),
            0x53 => self.ld(&RegD, &RegE),
            0x54 => self.ld(&RegD, &RegH),
            0x55 => self.ld(&RegD, &RegL),
            0x56 => self.ld(&RegD, &MemHL),
            0x57 => self.ld(&RegD, &RegA),
            0x58 => self.ld(&RegE, &RegB),
            0x59 => self.ld(&RegE, &RegC),
            0x5A => self.ld(&RegE, &RegD),
            0x5B => self.ld(&RegE, &RegE),
            0x5C => self.ld(&RegE, &RegH),
            0x5D => self.ld(&RegE, &RegL),
            0x5E => self.ld(&RegE, &MemHL),
            0x5F => self.ld(&RegE, &RegA),
            0x60 => self.ld(&RegH, &RegB),
            0x61 => self.ld(&RegH, &RegC),
            0x62 => self.ld(&RegH, &RegD),
            0x63 => self.ld(&RegH, &RegE),
            0x64 => self.ld(&RegH, &RegH),
            0x65 => self.ld(&RegH, &RegL),
            0x66 => self.ld(&RegH, &MemHL),
            0x67 => self.ld(&RegH, &RegA),
            0x68 => self.ld(&RegL, &RegB),
            0x69 => self.ld(&RegL, &RegC),
            0x6A => self.ld(&RegL, &RegD),
            0x6B => self.ld(&RegL, &RegE),
            0x6C => self.ld(&RegL, &RegH),
            0x6D => self.ld(&RegL, &RegL),
            0x6E => self.ld(&RegL, &MemHL),
            0x6F => self.ld(&RegL, &RegA),
            0x70 => self.ld(&MemHL, &RegB),
            0x71 => self.ld(&MemHL, &RegC),
            0x72 => self.ld(&MemHL, &RegD),
            0x73 => self.ld(&MemHL, &RegE),
            0x74 => self.ld(&MemHL, &RegH),
            0x75 => self.ld(&MemHL, &RegL),
            0x76 => self.halt(),
            0x77 => self.ld(&MemHL, &RegA),
            0x78 => self.ld(&RegA, &RegB),
            0x79 => self.ld(&RegA, &RegC),
            0x7A => self.ld(&RegA, &RegD),
            0x7B => self.ld(&RegA, &RegE),
            0x7C => self.ld(&RegA, &RegH),
            0x7D => self.ld(&RegA, &RegL),
            0x7E => self.ld(&RegA, &MemHL),
            0x7F => self.ld(&RegA, &RegA),
            0x80 => self.add(&RegB),
            0x81 => self.add(&RegC),
            0x82 => self.add(&RegD),
            0x83 => self.add(&RegE),
            0x84 => self.add(&RegH),
            0x85 => self.add(&RegL),
            0x86 => self.add(&MemHL),
            0x87 => self.add(&RegA),
            0x88 => self.adc(&RegB),
            0x89 => self.adc(&RegC),
            0x8A => self.adc(&RegD),
            0x8B => self.adc(&RegE),
            0x8C => self.adc(&RegH),
            0x8D => self.adc(&RegL),
            0x8E => self.adc(&MemHL),
            0x8F => self.adc(&RegA),
            0x90 => self.sub(&RegB),
            0x91 => self.sub(&RegC),
            0x92 => self.sub(&RegD),
            0x93 => self.sub(&RegE),
            0x94 => self.sub(&RegH),
            0x95 => self.sub(&RegL),
            0x96 => self.sub(&MemHL),
            0x97 => self.sub(&RegA),
            0x98 => self.sbc(&RegB),
            0x99 => self.sbc(&RegC),
            0x9A => self.sbc(&RegD),
            0x9B => self.sbc(&RegE),
            0x9C => self.sbc(&RegH),
            0x9D => self.sbc(&RegL),
            0x9E => self.sbc(&MemHL),
            0x9F => self.sbc(&RegA),
            0xA0 => self.and(&RegB),
            0xA1 => self.and(&RegC),
            0xA2 => self.and(&RegD),
            0xA3 => self.and(&RegE),
            0xA4 => self.and(&RegH),
            0xA5 => self.and(&RegL),
            0xA6 => self.and(&MemHL),
            0xA7 => self.and(&RegA),
            0xA8 => self.xor(&RegB),
            0xA9 => self.xor(&RegC),
            0xAA => self.xor(&RegD),
            0xAB => self.xor(&RegE),
            0xAC => self.xor(&RegH),
            0xAD => self.xor(&RegL),
            0xAE => self.xor(&MemHL),
            0xAF => self.xor(&RegA),
            0xB0 => self.or(&RegB),
            0xB1 => self.or(&RegC),
            0xB2 => self.or(&RegD),
            0xB3 => self.or(&RegE),
            0xB4 => self.or(&RegH),
            0xB5 => self.or(&RegL),
            0xB6 => self.or(&MemHL),
            0xB7 => self.or(&RegA),
            0xB8 => self.cp(&RegB),
            0xB9 => self.cp(&RegC),
            0xBA => self.cp(&RegD),
            0xBB => self.cp(&RegE),
            0xBC => self.cp(&RegH),
            0xBD => self.cp(&RegL),
            0xBE => self.cp(&MemHL),
            0xBF => self.cp(&RegA),
            0xC0 => self.ret_cc(FlagCondition::NotZero),
            0xC1 => self.pop(&RegBC),
            0xC2 => self.jp_cc(FlagCondition::NotZero),
            0xC3 => self.jp(&Immediate16),
            0xC4 => self.call_cc(FlagCondition::NotZero),
            0xC5 => self.push(&RegBC),
            0xC6 => self.add(&Immediate),
            0xC7 => self.rst(0x00),
            0xC8 => self.ret_cc(FlagCondition::Zero),
            0xC9 => self.ret(),
            0xCA => self.jp_cc(FlagCondition::Zero),
            0xCB => self.extended_op(),
            0xCC => self.call_cc(FlagCondition::Zero),
            0xCD => self.call(),
            0xCE => self.adc(&Immediate),
            0xCF => self.rst(0x08),
            0xD0 => self.ret_cc(FlagCondition::NotCarry),
            0xD1 => self.pop(&RegDE),
            0xD2 => self.jp_cc(FlagCondition::NotCarry),
            0xD4 => self.call_cc(FlagCondition::NotCarry),
            0xD5 => self.push(&RegDE),
            0xD6 => self.sub(&Immediate),
            0xD7 => self.rst(0x10),
            0xD8 => self.ret_cc(FlagCondition::Carry),
            0xD9 => self.reti(),
            0xDA => self.jp_cc(FlagCondition::Carry),
            0xDC => self.call_cc(FlagCondition::Carry),
            0xDE => self.sbc(&Immediate),
            0xDF => self.rst(0x18),
            0xE0 => self.ld(&MemImmediateHigh, &RegA),
            0xE1 => self.pop(&RegHL),
            0xE2 => self.ld(&MemCHigh, &RegA),
            0xE5 => self.push(&RegHL),
            0xE6 => self.and(&Immediate),
            0xE7 => self.rst(0x20),
            0xE8 => self.add_sp_n(),
            0xE9 => self.jp(&RegHL),
            0xEA => self.ld(&MemImmediate, &RegA),
            0xEE => self.xor(&Immediate),
            0xEF => self.rst(0x28),
            0xF0 => self.ld(&RegA, &MemImmediateHigh),
            0xF1 => self.pop_af(),
            0xF2 => self.ld(&RegA, &MemCHigh),
            0xF3 => self.di(),
            0xF5 => self.push(&RegAF),
            0xF6 => self.or(&Immediate),
            0xF7 => self.rst(0x30),
            0xF8 => self.ld_hl_sp_n(),
            0xF9 => self.ld16(&RegSP, &RegHL),
            0xFA => self.ld(&RegA, &MemImmediate),
            0xFB => self.ei(),
            0xFE => self.cp(&Immediate),
            0xFF => self.rst(0x38),
            _ => panic!("Unrecognised op code: {:X}", op)
        };

        self.clock += OP_TIMES[op as usize];
    }

    fn extended_op(&mut self) {
        let op = self.next_byte();

        match op {
            0x00 => self.rlc(&RegB),
            0x01 => self.rlc(&RegC),
            0x02 => self.rlc(&RegD),
            0x03 => self.rlc(&RegE),
            0x04 => self.rlc(&RegH),
            0x05 => self.rlc(&RegL),
            0x06 => self.rlc(&MemHL),
            0x07 => self.rlc(&RegA),
            0x08 => self.rrc(&RegB),
            0x09 => self.rrc(&RegC),
            0x0A => self.rrc(&RegD),
            0x0B => self.rrc(&RegE),
            0x0C => self.rrc(&RegH),
            0x0D => self.rrc(&RegL),
            0x0E => self.rrc(&MemHL),
            0x0F => self.rrc(&RegA),
            0x10 => self.rl(&RegB),
            0x11 => self.rl(&RegC),
            0x12 => self.rl(&RegD),
            0x13 => self.rl(&RegE),
            0x14 => self.rl(&RegH),
            0x15 => self.rl(&RegL),
            0x16 => self.rl(&MemHL),
            0x17 => self.rl(&RegA),
            0x18 => self.rr(&RegB),
            0x19 => self.rr(&RegC),
            0x1A => self.rr(&RegD),
            0x1B => self.rr(&RegE),
            0x1C => self.rr(&RegH),
            0x1D => self.rr(&RegL),
            0x1E => self.rr(&MemHL),
            0x1F => self.rr(&RegA),
            0x20 => self.sla(&RegB),
            0x21 => self.sla(&RegC),
            0x22 => self.sla(&RegD),
            0x23 => self.sla(&RegE),
            0x24 => self.sla(&RegH),
            0x25 => self.sla(&RegL),
            0x26 => self.sla(&MemHL),
            0x27 => self.sla(&RegA),
            0x28 => self.sra(&RegB),
            0x29 => self.sra(&RegC),
            0x2A => self.sra(&RegD),
            0x2B => self.sra(&RegE),
            0x2C => self.sra(&RegH),
            0x2D => self.sra(&RegL),
            0x2E => self.sra(&MemHL),
            0x2F => self.sra(&RegA),
            0x30 => self.swap(&RegB),
            0x31 => self.swap(&RegC),
            0x32 => self.swap(&RegD),
            0x33 => self.swap(&RegE),
            0x34 => self.swap(&RegH),
            0x35 => self.swap(&RegL),
            0x36 => self.swap(&MemHL),
            0x37 => self.swap(&RegA),
            0x38 => self.srl(&RegB),
            0x39 => self.srl(&RegC),
            0x3A => self.srl(&RegD),
            0x3B => self.srl(&RegE),
            0x3C => self.srl(&RegH),
            0x3D => self.srl(&RegL),
            0x3E => self.srl(&MemHL),
            0x3F => self.srl(&RegA),
            0x40 => self.bit(0, &RegB),
            0x41 => self.bit(0, &RegC),
            0x42 => self.bit(0, &RegD),
            0x43 => self.bit(0, &RegE),
            0x44 => self.bit(0, &RegH),
            0x45 => self.bit(0, &RegL),
            0x46 => self.bit(0, &MemHL),
            0x47 => self.bit(0, &RegA),
            0x48 => self.bit(1, &RegB),
            0x49 => self.bit(1, &RegC),
            0x4A => self.bit(1, &RegD),
            0x4B => self.bit(1, &RegE),
            0x4C => self.bit(1, &RegH),
            0x4D => self.bit(1, &RegL),
            0x4E => self.bit(1, &MemHL),
            0x4F => self.bit(1, &RegA),
            0x50 => self.bit(2, &RegB),
            0x51 => self.bit(2, &RegC),
            0x52 => self.bit(2, &RegD),
            0x53 => self.bit(2, &RegE),
            0x54 => self.bit(2, &RegH),
            0x55 => self.bit(2, &RegL),
            0x56 => self.bit(2, &MemHL),
            0x57 => self.bit(2, &RegA),
            0x58 => self.bit(3, &RegB),
            0x59 => self.bit(3, &RegC),
            0x5A => self.bit(3, &RegD),
            0x5B => self.bit(3, &RegE),
            0x5C => self.bit(3, &RegH),
            0x5D => self.bit(3, &RegL),
            0x5E => self.bit(3, &MemHL),
            0x5F => self.bit(3, &RegA),
            0x60 => self.bit(4, &RegB),
            0x61 => self.bit(4, &RegC),
            0x62 => self.bit(4, &RegD),
            0x63 => self.bit(4, &RegE),
            0x64 => self.bit(4, &RegH),
            0x65 => self.bit(4, &RegL),
            0x66 => self.bit(4, &MemHL),
            0x67 => self.bit(4, &RegA),
            0x68 => self.bit(5, &RegB),
            0x69 => self.bit(5, &RegC),
            0x6A => self.bit(5, &RegD),
            0x6B => self.bit(5, &RegE),
            0x6C => self.bit(5, &RegH),
            0x6D => self.bit(5, &RegL),
            0x6E => self.bit(5, &MemHL),
            0x6F => self.bit(5, &RegA),
            0x70 => self.bit(6, &RegB),
            0x71 => self.bit(6, &RegC),
            0x72 => self.bit(6, &RegD),
            0x73 => self.bit(6, &RegE),
            0x74 => self.bit(6, &RegH),
            0x75 => self.bit(6, &RegL),
            0x76 => self.bit(6, &MemHL),
            0x77 => self.bit(6, &RegA),
            0x78 => self.bit(7, &RegB),
            0x79 => self.bit(7, &RegC),
            0x7A => self.bit(7, &RegD),
            0x7B => self.bit(7, &RegE),
            0x7C => self.bit(7, &RegH),
            0x7D => self.bit(7, &RegL),
            0x7E => self.bit(7, &MemHL),
            0x7F => self.bit(7, &RegA),
            0x80 => self.res(0, &RegB),
            0x81 => self.res(0, &RegC),
            0x82 => self.res(0, &RegD),
            0x83 => self.res(0, &RegE),
            0x84 => self.res(0, &RegH),
            0x85 => self.res(0, &RegL),
            0x86 => self.res(0, &MemHL),
            0x87 => self.res(0, &RegA),
            0x88 => self.res(1, &RegB),
            0x89 => self.res(1, &RegC),
            0x8A => self.res(1, &RegD),
            0x8B => self.res(1, &RegE),
            0x8C => self.res(1, &RegH),
            0x8D => self.res(1, &RegL),
            0x8E => self.res(1, &MemHL),
            0x8F => self.res(1, &RegA),
            0x90 => self.res(2, &RegB),
            0x91 => self.res(2, &RegC),
            0x92 => self.res(2, &RegD),
            0x93 => self.res(2, &RegE),
            0x94 => self.res(2, &RegH),
            0x95 => self.res(2, &RegL),
            0x96 => self.res(2, &MemHL),
            0x97 => self.res(2, &RegA),
            0x98 => self.res(3, &RegB),
            0x99 => self.res(3, &RegC),
            0x9A => self.res(3, &RegD),
            0x9B => self.res(3, &RegE),
            0x9C => self.res(3, &RegH),
            0x9D => self.res(3, &RegL),
            0x9E => self.res(3, &MemHL),
            0x9F => self.res(3, &RegA),
            0xA0 => self.res(4, &RegB),
            0xA1 => self.res(4, &RegC),
            0xA2 => self.res(4, &RegD),
            0xA3 => self.res(4, &RegE),
            0xA4 => self.res(4, &RegH),
            0xA5 => self.res(4, &RegL),
            0xA6 => self.res(4, &MemHL),
            0xA7 => self.res(4, &RegA),
            0xA8 => self.res(5, &RegB),
            0xA9 => self.res(5, &RegC),
            0xAA => self.res(5, &RegD),
            0xAB => self.res(5, &RegE),
            0xAC => self.res(5, &RegH),
            0xAD => self.res(5, &RegL),
            0xAE => self.res(5, &MemHL),
            0xAF => self.res(5, &RegA),
            0xB0 => self.res(6, &RegB),
            0xB1 => self.res(6, &RegC),
            0xB2 => self.res(6, &RegD),
            0xB3 => self.res(6, &RegE),
            0xB4 => self.res(6, &RegH),
            0xB5 => self.res(6, &RegL),
            0xB6 => self.res(6, &MemHL),
            0xB7 => self.res(6, &RegA),
            0xB8 => self.res(7, &RegB),
            0xB9 => self.res(7, &RegC),
            0xBA => self.res(7, &RegD),
            0xBB => self.res(7, &RegE),
            0xBC => self.res(7, &RegH),
            0xBD => self.res(7, &RegL),
            0xBE => self.res(7, &MemHL),
            0xBF => self.res(7, &RegA),
            0xC0 => self.set(0, &RegB),
            0xC1 => self.set(0, &RegC),
            0xC2 => self.set(0, &RegD),
            0xC3 => self.set(0, &RegE),
            0xC4 => self.set(0, &RegH),
            0xC5 => self.set(0, &RegL),
            0xC6 => self.set(0, &MemHL),
            0xC7 => self.set(0, &RegA),
            0xC8 => self.set(1, &RegB),
            0xC9 => self.set(1, &RegC),
            0xCA => self.set(1, &RegD),
            0xCB => self.set(1, &RegE),
            0xCC => self.set(1, &RegH),
            0xCD => self.set(1, &RegL),
            0xCE => self.set(1, &MemHL),
            0xCF => self.set(1, &RegA),
            0xD0 => self.set(2, &RegB),
            0xD1 => self.set(2, &RegC),
            0xD2 => self.set(2, &RegD),
            0xD3 => self.set(2, &RegE),
            0xD4 => self.set(2, &RegH),
            0xD5 => self.set(2, &RegL),
            0xD6 => self.set(2, &MemHL),
            0xD7 => self.set(2, &RegA),
            0xD8 => self.set(3, &RegB),
            0xD9 => self.set(3, &RegC),
            0xDA => self.set(3, &RegD),
            0xDB => self.set(3, &RegE),
            0xDC => self.set(3, &RegH),
            0xDD => self.set(3, &RegL),
            0xDE => self.set(3, &MemHL),
            0xDF => self.set(3, &RegA),
            0xE0 => self.set(4, &RegB),
            0xE1 => self.set(4, &RegC),
            0xE2 => self.set(4, &RegD),
            0xE3 => self.set(4, &RegE),
            0xE4 => self.set(4, &RegH),
            0xE5 => self.set(4, &RegL),
            0xE6 => self.set(4, &MemHL),
            0xE7 => self.set(4, &RegA),
            0xE8 => self.set(5, &RegB),
            0xE9 => self.set(5, &RegC),
            0xEA => self.set(5, &RegD),
            0xEB => self.set(5, &RegE),
            0xEC => self.set(5, &RegH),
            0xED => self.set(5, &RegL),
            0xEE => self.set(5, &MemHL),
            0xEF => self.set(5, &RegA),
            0xF0 => self.set(6, &RegB),
            0xF1 => self.set(6, &RegC),
            0xF2 => self.set(6, &RegD),
            0xF3 => self.set(6, &RegE),
            0xF4 => self.set(6, &RegH),
            0xF5 => self.set(6, &RegL),
            0xF6 => self.set(6, &MemHL),
            0xF7 => self.set(6, &RegA),
            0xF8 => self.set(7, &RegB),
            0xF9 => self.set(7, &RegC),
            0xFA => self.set(7, &RegD),
            0xFB => self.set(7, &RegE),
            0xFC => self.set(7, &RegH),
            0xFD => self.set(7, &RegL),
            0xFE => self.set(7, &MemHL),
            0xFF => self.set(7, &RegA),
            _ => panic!("Unrecognised op code: CB {:X}", op)
        };

        self.clock += EXTENDED_OP_TIMES[op as usize];
    }

    fn next_byte(&mut self) -> Byte {
        let byte = self.mmu.byte(self.pc);
        self.pc += 1;
        byte
    }

    fn get_flag(&self, flag: Byte) -> bool {
        self.af.low & flag != 0
    }

    fn set_flag(&mut self, flag: Byte, value: bool) {
        if value {
            self.af.low |= flag;
        } else {
            self.af.low &= !flag;
        }
    }

    fn stack_push(&mut self, value: Word) {
        self.sp -= 2;
        self.mmu.put_word(self.sp, value);
    }

    fn stack_pop(&mut self) -> Word {
        let value = self.mmu.word(self.sp);
        self.sp += 2;
        value
    }

    fn ld(&mut self, lhs: &Writer<Byte>, rhs: &Reader<Byte>) {
        trace!("LD {},{}", lhs, rhs);
        let value = rhs.read(self);
        lhs.write(self, value);
    }

    fn ldi(&mut self, lhs: &Writer<Byte>, rhs: &Reader<Byte>) {
        trace!("LDI {},{}", lhs, rhs);
        let value = rhs.read(self);
        lhs.write(self, value);
        self.hl += 1;
    }

    fn ldd(&mut self, lhs: &Writer<Byte>, rhs: &Reader<Byte>) {
        trace!("LDD {},{}", lhs, rhs);
        let value = rhs.read(self);
        lhs.write(self, value);
        self.hl -= 1;
    }

    fn ld16(&mut self, lhs: &Writer<Word>, rhs: &Reader<Word>) {
        trace!("LD {},{}", lhs, rhs);
        let value = rhs.read(self);
        lhs.write(self, value);
    }

    fn ld_hl_sp_n(&mut self) {
        trace!("LD HL,SP+n");
        let lhs = self.sp.get();
        let rhs = self.next_byte() as u16;

        let result = if rhs > 127 {
            lhs.wrapping_sub(256 - rhs)
        } else {
            lhs.wrapping_add(rhs)
        };

        self.set_flag(ZERO, false);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, ((lhs & 0x000F) + (rhs & 0x000F)) > 0x000F);
        self.set_flag(CARRY, ((lhs & 0x00FF) + (rhs & 0x00FF)) > 0x00FF);
        self.hl = Word::new(result);
    }

    fn push(&mut self, rhs: &Reader<Word>) {
        trace!("PUSH {}", rhs);
        let value = rhs.read(self);
        self.stack_push(value);
    }

    fn pop(&mut self, rhs: &Writer<Word>) {
        trace!("POP {}", rhs);
        let value = self.stack_pop();
        rhs.write(self, value);
    }

    fn pop_af(&mut self) {
        trace!("POP AF");
        let value = self.stack_pop();
        // Only the upper nibble is actually written back to AF
        // (This means it's different from other POPs)
        self.af.high = value.high;
        self.af.low = value.low & 0xF0;
    }

    fn add(&mut self, rhs: &Reader<Byte>) {
        trace!("ADD A,{}", rhs);
        let lhs = self.af.high;
        let result = lhs.wrapping_add(rhs.read(self));
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, (result & 0x0F) < (lhs & 0x0F));
        self.set_flag(CARRY, result < lhs);
        self.af.high = result;
    }

    fn adc(&mut self, rhs: &Reader<Byte>) {
        trace!("ADC A,{}", rhs);
        let lhs = self.af.high;
        let mut result = lhs.wrapping_add(rhs.read(self));
        let mut half_carry = (result & 0x0F) < (lhs & 0x0F);
        let mut carry = result < lhs;

        if self.get_flag(CARRY) {
            result = result.wrapping_add(1);
            half_carry = half_carry || (result & 0x0F) == 0;
            carry = carry || result == 0;
        }

        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, half_carry);
        self.set_flag(CARRY, carry);
        self.af.high = result;
    }

    fn sub(&mut self, rhs: &Reader<Byte>) {
        trace!("SUB A,{}", rhs);
        let lhs = self.af.high;
        let result = lhs.wrapping_sub(rhs.read(self));
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, true);
        self.set_flag(HALF_CARRY, (result & 0x0F) > (lhs & 0x0F));
        self.set_flag(CARRY, result > lhs);
        self.af.high = result;
    }

    fn sbc(&mut self, rhs: &Reader<Byte>) {
        trace!("SBC A,{}", rhs);
        let lhs = self.af.high;
        let mut result = lhs.wrapping_sub(rhs.read(self));
        let mut half_carry = (result & 0x0F) > (lhs & 0x0F);
        let mut carry = result > lhs;

        if self.get_flag(CARRY) {
            result = result.wrapping_sub(1);
            half_carry = half_carry || (result & 0x0F) == 0x0F;
            carry = carry || result == 0xFF;
        }

        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, true);
        self.set_flag(HALF_CARRY, half_carry);
        self.set_flag(CARRY, carry);
        self.af.high = result;
    }

    fn and(&mut self, rhs: &Reader<Byte>) {
        trace!("AND A,{}", rhs);
        let lhs = self.af.high;
        let result = lhs & rhs.read(self);
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, true);
        self.set_flag(CARRY, false);
        self.af.high = result;
    }

    fn or(&mut self, rhs: &Reader<Byte>) {
        trace!("OR A,{}", rhs);
        let lhs = self.af.high;
        let result = lhs | rhs.read(self);
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, false);
        self.set_flag(CARRY, false);
        self.af.high = result;
    }

    fn xor(&mut self, rhs: &Reader<Byte>) {
        trace!("XOR A,{}", rhs);
        let lhs = self.af.high;
        let result = lhs ^ rhs.read(self);
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, false);
        self.set_flag(CARRY, false);
        self.af.high = result;
    }

    fn cp(&mut self, rhs: &Reader<Byte>) {
        trace!("CP A,{}", rhs);
        let lhs = self.af.high;
        let result = lhs.wrapping_sub(rhs.read(self));
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, true);
        self.set_flag(HALF_CARRY, (result & 0x0F) > (lhs & 0x0F));
        self.set_flag(CARRY, result > lhs);
    }

    fn inc(&mut self, addr: &Writer<Byte>) {
        trace!("INC {}", addr);
        let result = addr.read(self).wrapping_add(1);
        addr.write(self, result);
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, (result & 0x0F) == 0);
    }

    fn dec(&mut self, addr: &Writer<Byte>) {
        trace!("DEC {}", addr);
        let result = addr.read(self).wrapping_sub(1);
        addr.write(self, result);
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, true);
        self.set_flag(HALF_CARRY, (result & 0x0F) == 0x0F);
    }

    fn add16(&mut self, rhs: &Reader<Word>) {
        trace!("ADD HL,{}", rhs);
        let lhs = self.hl.get();
        let result = lhs.wrapping_add(rhs.read(self).get());
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, (result & 0x0FFF) < (lhs & 0x0FFF));
        self.set_flag(CARRY, result < lhs);
        self.hl.set(result);
    }

    fn add_sp_n(&mut self) {
        trace!("ADD SP,n");
        let lhs = self.sp.get();
        let rhs = self.next_byte() as u16;

        let result = if rhs > 127 {
            lhs.wrapping_sub(256 - rhs)
        } else {
            lhs.wrapping_add(rhs)
        };

        self.set_flag(ZERO, false);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, ((lhs & 0x000F) + (rhs & 0x000F)) > 0x000F);
        self.set_flag(CARRY, ((lhs & 0x00FF) + (rhs & 0x00FF)) > 0x00FF);
        self.sp = Word::new(result);
    }

    fn inc16(&mut self, addr: &Writer<Word>) {
        trace!("INC {}", addr);
        let result = addr.read(self).get().wrapping_add(1);
        addr.write(self, Word::new(result));
    }

    fn dec16(&mut self, addr: &Writer<Word>) {
        trace!("DEC {}", addr);
        let result = addr.read(self).get().wrapping_sub(1);
        addr.write(self, Word::new(result));
    }

    fn swap(&mut self, addr: &Writer<Byte>) {
        trace!("SWAP {}", addr);
        let value = addr.read(self);
        let result = ((value & 0xF0) >> 4) + ((value & 0x0F) << 4);
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, false);
        self.set_flag(CARRY, false);
        addr.write(self, result);
    }

    fn daa(&mut self) {
        trace!("DAA");

        let mut result = self.af.high;

        if self.get_flag(SUBTRACT) {
            if self.get_flag(HALF_CARRY) {
                result = result.wrapping_sub(0x06);
            }

            if self.get_flag(CARRY) {
                result = result.wrapping_sub(0x60);
            }

        } else {
            if (result & 0x0F) > 0x09 || self.get_flag(HALF_CARRY) {
                result = result.wrapping_add(0x06);

                if result < self.af.high {
                    self.set_flag(CARRY, true);
                }
            }

            if (result & 0xF0) > 0x90 || self.get_flag(CARRY) {
                result = result.wrapping_add(0x60);

                if result < self.af.high {
                    self.set_flag(CARRY, true);
                }
            }
        };

        self.set_flag(ZERO, result == 0);
        self.set_flag(HALF_CARRY, false);

        self.af.high = result;
    }

    fn cpl(&mut self) {
        trace!("CPL");
        self.af.high = !self.af.high;
        self.set_flag(SUBTRACT, true);
        self.set_flag(HALF_CARRY, true);
    }

    fn ccf(&mut self) {
        trace!("CCF");
        let carry = self.get_flag(CARRY);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, false);
        self.set_flag(CARRY, !carry);
    }

    fn scf(&mut self) {
        trace!("SCF");
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, false);
        self.set_flag(CARRY, true);
    }

    fn nop(&mut self) {
        trace!("NOP");
    }

    fn halt(&mut self) {
        trace!("HALT");

        // Only halt when IME is enabled or when there is no currently pending interrupt
        if self.ime || self.interrupt_state.borrow().next_interrupt().is_none() {
            self.halted = true;
        } else {
            // Otherwise, there is a bug that causes the program counter to
            // freeze on the next instruction. But we'll avoid that mess and
            // just increment the program counter so we skip it.
            self.pc += 1;
        }
    }

    fn stop(&mut self) {
        trace!("STOP");

        // Only stop if the next instruction is a NOP ($00)
        if Immediate.read(self) == 0x00 {
            self.stopped = true;
        }
    }

    fn di(&mut self) {
        trace!("DI");
        self.ime = false;
    }

    fn ei(&mut self) {
        trace!("EI");
        // Enable interrupts only after the next instruction
        self.ime_delayed = true;
    }

    fn rlca(&mut self) {
        trace!("RLCA");
        let lhs = self.af.high;
        let result = lhs.rotate_left(1);
        self.set_flag(ZERO, false);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, false);
        self.set_flag(CARRY, (lhs & 0x80) != 0);
        self.af.high = result;
    }

    fn rla(&mut self) {
        trace!("RLA");
        let lhs = self.af.high;
        let result = ((lhs & 0x7F) << 1) + self.get_flag(CARRY) as u8;
        self.set_flag(ZERO, false);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, false);
        self.set_flag(CARRY, (lhs & 0x80) != 0);
        self.af.high = result;
    }

    fn rrca(&mut self) {
        trace!("RRCA");
        let lhs = self.af.high;
        let result = lhs.rotate_right(1);
        self.set_flag(ZERO, false);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, false);
        self.set_flag(CARRY, (lhs & 0x01) != 0);
        self.af.high = result;
    }

    fn rra(&mut self) {
        trace!("RRA");
        let lhs = self.af.high;
        let result = ((self.get_flag(CARRY) as u8) << 7) + ((lhs & 0xFE) >> 1);
        self.set_flag(ZERO, false);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, false);
        self.set_flag(CARRY, (lhs & 0x01) != 0);
        self.af.high = result;
    }

    fn rlc(&mut self, rhs: &Writer<Byte>) {
        trace!("RLC {}", rhs);
        let lhs = rhs.read(self);
        let result = lhs.rotate_left(1);
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, false);
        self.set_flag(CARRY, (lhs & 0x80) != 0);
        rhs.write(self, result);
    }

    fn rl(&mut self, rhs: &Writer<Byte>) {
        trace!("RL {}", rhs);
        let lhs = rhs.read(self);
        let result = ((lhs & 0x7F) << 1) + self.get_flag(CARRY) as u8;
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, false);
        self.set_flag(CARRY, (lhs & 0x80) != 0);
        rhs.write(self, result);
    }

    fn rrc(&mut self, rhs: &Writer<Byte>) {
        trace!("RRC {}", rhs);
        let lhs = rhs.read(self);
        let result = lhs.rotate_right(1);
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, false);
        self.set_flag(CARRY, (lhs & 0x01) != 0);
        rhs.write(self, result);
    }

    fn rr(&mut self, rhs: &Writer<Byte>) {
        trace!("RR {}", rhs);
        let lhs = rhs.read(self);
        let result = ((self.get_flag(CARRY) as u8) << 7) + ((lhs & 0xFE) >> 1);
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, false);
        self.set_flag(CARRY, (lhs & 0x01) != 0);
        rhs.write(self, result);
    }

    fn sla(&mut self, rhs: &Writer<Byte>) {
        trace!("SLA {}", rhs);
        let lhs = rhs.read(self);
        let result = (lhs & 0x7F) << 1;
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, false);
        self.set_flag(CARRY, (lhs & 0x80) != 0);
        rhs.write(self, result);
    }

    fn sra(&mut self, rhs: &Writer<Byte>) {
        trace!("SRA {}", rhs);
        let lhs = rhs.read(self);
        let result = ((lhs & 0xFE) >> 1) + (lhs & 0x80);
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, false);
        self.set_flag(CARRY, (lhs & 0x01) != 0);
        rhs.write(self, result);
    }

    fn srl(&mut self, rhs: &Writer<Byte>) {
        trace!("SRL {}", rhs);
        let lhs = rhs.read(self);
        let result = (lhs & 0xFE) >> 1;
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, false);
        self.set_flag(CARRY, (lhs & 0x01) != 0);
        rhs.write(self, result);
    }

    fn bit(&mut self, bit: u8, rhs: &Writer<Byte>) {
        trace!("BIT {},{}", bit, rhs);
        let value = rhs.read(self);
        self.set_flag(ZERO, (value & (0x01 << bit)) == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(HALF_CARRY, true);
    }

    fn set(&mut self, bit: u8, rhs: &Writer<Byte>) {
        trace!("SET {},{}", bit, rhs);
        let value = rhs.read(self);
        rhs.write(self, value | (0x01 << bit));
    }

    fn res(&mut self, bit: u8, rhs: &Writer<Byte>) {
        trace!("RES {},{}", bit, rhs);
        let value = rhs.read(self);
        rhs.write(self, value & !(0x01 << bit));
    }

    fn jp(&mut self, rhs: &Reader<Word>) {
        trace!("JP {}", rhs);
        let address = rhs.read(self);
        self.pc = address;
    }

    fn jp_cc(&mut self, flag_condition: FlagCondition) {
        trace!("JP {},nn", flag_condition);

        if flag_condition.apply(self) {
            let address = Immediate16.read(self);
            self.pc = address;
            self.clock += 1;
        } else {
            self.pc += 2;
        }
    }

    fn jr(&mut self) {
        trace!("JR n");

        let offset = Immediate.read(self) as i8;

        // By first converting to i8 and then to i16, we force the
        // 8-bit value to become signed
        let address = (self.pc.get() as i16).wrapping_add(offset as i16) as u16;
        self.pc.set(address);
    }

    fn jr_cc(&mut self, flag_condition: FlagCondition) {
        trace!("JR {},n", flag_condition);

        if flag_condition.apply(self) {
            // By first converting to i8 and then to i16, we force the
            // 8-bit value to become signed
            let offset = Immediate.read(self) as i8;
            let address = (self.pc.get() as i16).wrapping_add(offset as i16) as u16;
            self.pc.set(address);
            self.clock += 1;
        } else {
            self.pc += 1;
        }
    }

    fn call(&mut self) {
        trace!("CALL nn");

        let address = Immediate16.read(self);

        let value = self.pc;
        self.stack_push(value);
        self.pc = address;
    }

    fn call_cc(&mut self, flag_condition: FlagCondition) {
        trace!("CALL {},nn", flag_condition);

        if flag_condition.apply(self) {
            let address = Immediate16.read(self);
            let value = self.pc;
            self.stack_push(value);
            self.pc = address;
            self.clock += 3;
        } else {
            self.pc += 2;
        }
    }

    fn rst(&mut self, address: Byte) {
        trace!("RST ${:02X}", address);
        let value = self.pc;
        self.stack_push(value);
        self.pc.high = 0x00;
        self.pc.low = address;
    }

    fn ret(&mut self) {
        trace!("RET");
        self.pc = self.stack_pop();
    }

    fn ret_cc(&mut self, flag_condition: FlagCondition) {
        trace!("RET {}", flag_condition);

        if flag_condition.apply(self) {
            self.pc = self.stack_pop();
            self.clock += 3;
        }
    }

    fn reti(&mut self) {
        trace!("RETI");
        self.pc = self.stack_pop();
        self.ime = true;
    }
}

impl FlagCondition {
    pub fn apply(&self, cpu: &mut Cpu) -> bool {
        match *self {
            FlagCondition::NotZero => !cpu.get_flag(ZERO),
            FlagCondition::Zero => cpu.get_flag(ZERO),
            FlagCondition::NotCarry => !cpu.get_flag(CARRY),
            FlagCondition::Carry => cpu.get_flag(CARRY)
        }
    }
}

impl Display for FlagCondition {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", match *self {
            FlagCondition::NotZero => "NZ",
            FlagCondition::Zero => "Z",
            FlagCondition::NotCarry => "NC",
            FlagCondition::Carry => "C"
        })
    }
}

impl Reader<Byte> for Immediate {
    fn read(&self, cpu: &mut Cpu) -> Byte {
        let value = cpu.mmu.byte(cpu.pc);
        cpu.pc += 1;
        value
    }
}

impl Display for Immediate {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "n")
    }
}

impl Reader<Word> for Immediate16 {
    fn read(&self, cpu: &mut Cpu) -> Word {
        let value = cpu.mmu.word(cpu.pc);
        cpu.pc += 2;
        value
    }
}

impl Display for Immediate16 {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "nn")
    }
}

impl Reader<Byte> for MemImmediate {
    fn read(&self, cpu: &mut Cpu) -> Byte {
        let address = cpu.mmu.word(cpu.pc);
        cpu.pc += 2;
        cpu.mmu.byte(address)
    }
}

impl Writer<Byte> for MemImmediate {
    fn write(&self, cpu: &mut Cpu, value: Byte) {
        let address = cpu.mmu.word(cpu.pc);
        cpu.pc += 2;
        cpu.mmu.put_byte(address, value);
    }
}

impl Display for MemImmediate {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "(nn)")
    }
}

impl Reader<Word> for MemImmediate16 {
    fn read(&self, cpu: &mut Cpu) -> Word {
        let address = cpu.mmu.word(cpu.pc);
        cpu.pc += 2;
        cpu.mmu.word(address)
    }
}

impl Writer<Word> for MemImmediate16 {
    fn write(&self, cpu: &mut Cpu, value: Word) {
        let address = cpu.mmu.word(cpu.pc);
        cpu.pc += 2;
        cpu.mmu.put_word(address, value);
    }
}

impl Display for MemImmediate16 {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "(nn)")
    }
}

impl Reader<Byte> for MemCHigh {
    fn read(&self, cpu: &mut Cpu) -> Byte {
        cpu.mmu.high_byte(cpu.bc.low)
    }
}

impl Writer<Byte> for MemCHigh {
    fn write(&self, cpu: &mut Cpu, value: Byte) {
        cpu.mmu.put_high_byte(cpu.bc.low, value);
    }
}

impl Display for MemCHigh {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "($FF00+C)")
    }
}

impl Reader<Byte> for MemImmediateHigh {
    fn read(&self, cpu: &mut Cpu) -> Byte {
        let address = cpu.mmu.byte(cpu.pc);
        cpu.pc += 1;
        cpu.mmu.high_byte(address)
    }
}

impl Writer<Byte> for MemImmediateHigh {
    fn write(&self, cpu: &mut Cpu, value: Byte) {
        let address = cpu.mmu.byte(cpu.pc);
        cpu.pc += 1;
        cpu.mmu.put_high_byte(address, value);
    }
}

impl Display for MemImmediateHigh {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "($FF00+n)")
    }
}
