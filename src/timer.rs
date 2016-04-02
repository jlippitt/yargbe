use interrupt::{Interrupt, InterruptState};
use memory::Byte;
use std::cell::RefCell;
use std::rc::Rc;

pub struct Timer {
    timer_state: Rc<RefCell<TimerState>>,
    interrupt_state: Rc<RefCell<InterruptState>>,
    clock: u64
}

pub struct TimerState {
    divider: Byte,
    counter: Byte,
    modulo: Byte,
    control: Control
}

bitflags! {
    flags Control: u8 {
        const TIMER_RUNNING = 0x04,
        const TIMER_SPEED = 0x03
    }
}

impl Timer {
    pub fn new(
        timer_state: Rc<RefCell<TimerState>>,
        interrupt_state: Rc<RefCell<InterruptState>>
    ) -> Timer {
        Timer {
            timer_state: timer_state,
            interrupt_state: interrupt_state,
            clock: 0
        }
    }

    pub fn step(&mut self, delta: u64) {
        let mut timer_state = self.timer_state.borrow_mut();

        let threshold = match (timer_state.control & TIMER_SPEED).bits() {
            0x00 => 256,
            0x01 => 4,
            0x02 => 16,
            0x03 => 64,
            _ => unreachable!()
        };

        for _ in 0..delta {
            self.clock += 1;

            if self.clock % 64 == 0 {
                timer_state.divider = timer_state.divider.wrapping_add(1);
            }

            if timer_state.control.contains(TIMER_RUNNING) && self.clock % threshold == 0 {
                timer_state.counter = timer_state.counter.wrapping_add(1);

                // On overflow, set counter to module and fire interrupt
                if timer_state.counter == 0 {
                    timer_state.counter = timer_state.modulo;
                    self.interrupt_state.borrow_mut().fire(Interrupt::Timer);
                }
            }
        }
    }
}

impl TimerState {
    pub fn new() -> TimerState {
        TimerState {
            divider: 0x00,
            counter: 0x00,
            modulo: 0x00,
            control: Control::empty()
        }
    }

    pub fn byte(&self, offset: usize) -> Byte {
        match offset {
            0x04 => self.divider,
            0x05 => self.counter,
            0x06 => self.modulo,
            0x07 => self.control.bits(),
            _ => unreachable!()
        }
    }

    pub fn put_byte(&mut self, offset: usize, value: Byte) {
        match offset {
            // Note: Any write sets divider to zero
            0x04 => self.divider = 0,
            0x05 => self.counter = value,
            0x06 => self.modulo = value,
            0x07 => self.control = Control::from_bits_truncate(value),
            _ => unreachable!()
        };
    }
}
