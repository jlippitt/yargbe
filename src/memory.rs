use std::ops::{Add, AddAssign, Sub, SubAssign};

pub type Byte = u8;

#[derive(Clone, Copy, Default, Debug, Eq, PartialEq)]
pub struct Word {
    pub high: Byte,
    pub low: Byte
}

impl Word {
    pub fn new(value: u16) -> Word {
        Word {
            high: ((value & 0xFF00) >> 8) as u8,
            low: value as u8
        }
    }

    pub fn from_bytes(high: u8, low: u8) -> Word {
        Word {
            high: high,
            low: low
        }
    }

    pub fn get(&self) -> u16 {
        ((self.high as u16) << 8) + self.low as u16
    }

    pub fn set(&mut self, value: u16) {
        self.high = ((value & 0xFF00) >> 8) as u8;
        self.low = value as u8;
    }
}

impl Add<u16> for Word {
    type Output = Word;

    fn add(self, rhs: u16) -> Word {
        Word::new(self.get().wrapping_add(rhs))
    }
}

impl AddAssign<u16> for Word {
    fn add_assign(&mut self, rhs: u16) {
        let result = self.get().wrapping_add(rhs);
        self.set(result);
    }
}

impl Sub<u16> for Word {
    type Output = Word;

    fn sub(self, rhs: u16) -> Word {
        Word::new(self.get().wrapping_sub(rhs))
    }
}

impl SubAssign<u16> for Word {
    fn sub_assign(&mut self, rhs: u16) {
        let result = self.get().wrapping_sub(rhs);
        self.set(result);
    }
}
