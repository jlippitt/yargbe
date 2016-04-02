use sdl2::keyboard::Keycode;
use std::cell::RefCell;
use std::rc::Rc;
use interrupt::{Interrupt, InterruptState};
use memory::Byte;

pub struct Joypad {
    joypad_state: Rc<RefCell<JoypadState>>,
    interrupt_state: Rc<RefCell<InterruptState>>
}

pub struct JoypadState {
    column: ColumnFlags,
    dpad: DpadFlags,
    button: ButtonFlags
}

bitflags! {
    flags ColumnFlags: u8 {
        const COLUMN_DPAD = 0x20,
        const COLUMN_BUTTON = 0x10
    }
}

bitflags! {
    flags DpadFlags: u8 {
        const DPAD_DOWN = 0x08,
        const DPAD_UP = 0x04,
        const DPAD_LEFT = 0x02,
        const DPAD_RIGHT = 0x01
    }
}

bitflags! {
    flags ButtonFlags: u8 {
        const BUTTON_START = 0x08,
        const BUTTON_SELECT = 0x04,
        const BUTTON_B = 0x02,
        const BUTTON_A = 0x01
    }
}

impl Joypad {
    pub fn new(
        joypad_state: Rc<RefCell<JoypadState>>,
        interrupt_state: Rc<RefCell<InterruptState>>
    ) -> Joypad {
        Joypad {
            joypad_state: joypad_state,
            interrupt_state: interrupt_state
        }
    }

    pub fn key_down(&mut self, key: Keycode) {
        let mut joypad_state = self.joypad_state.borrow_mut();

        match key {
            Keycode::Down => joypad_state.dpad.remove(DPAD_DOWN),
            Keycode::Up => joypad_state.dpad.remove(DPAD_UP),
            Keycode::Left => joypad_state.dpad.remove(DPAD_LEFT),
            Keycode::Right => joypad_state.dpad.remove(DPAD_RIGHT),
            Keycode::Return => joypad_state.button.remove(BUTTON_START),
            Keycode::Space => joypad_state.button.remove(BUTTON_SELECT),
            Keycode::X => joypad_state.button.remove(BUTTON_B),
            Keycode::Z => joypad_state.button.remove(BUTTON_A),
            // Skip the interrupt if unrecognised key
            _ => return
        }

        self.interrupt_state.borrow_mut().fire(Interrupt::Joypad);
    }

    pub fn key_up(&mut self, key: Keycode) {
        let mut joypad_state = self.joypad_state.borrow_mut();

        match key {
            Keycode::Down => joypad_state.dpad.insert(DPAD_DOWN),
            Keycode::Up => joypad_state.dpad.insert(DPAD_UP),
            Keycode::Left => joypad_state.dpad.insert(DPAD_LEFT),
            Keycode::Right => joypad_state.dpad.insert(DPAD_RIGHT),
            Keycode::Return => joypad_state.button.insert(BUTTON_START),
            Keycode::Space => joypad_state.button.insert(BUTTON_SELECT),
            Keycode::X => joypad_state.button.insert(BUTTON_B),
            Keycode::Z => joypad_state.button.insert(BUTTON_A),
            // Skip the interrupt if unrecognised key
            _ => return
        }

        self.interrupt_state.borrow_mut().fire(Interrupt::Joypad);
    }
}

impl JoypadState {
    pub fn new() -> JoypadState {
        JoypadState {
            column: ColumnFlags::empty(),
            dpad: DpadFlags::all(),
            button: ButtonFlags::all()
        }
    }

    pub fn byte(&self) -> Byte {
        match self.column {
            COLUMN_DPAD => self.dpad.bits(),
            COLUMN_BUTTON => self.button.bits(),
            _ => 0x0F
        }
    }

    pub fn put_byte(&mut self, value: Byte) {
        self.column = ColumnFlags::from_bits_truncate(value);
    }
}
