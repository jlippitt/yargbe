#[macro_use]
extern crate bitflags;

#[cfg(feature="logging")]
#[macro_use]
extern crate log;

extern crate getopts;
extern crate memmap;
extern crate sdl2;
extern crate time;

#[macro_use]
mod logger;

mod audio;
mod config;
mod cpu;
mod gpu;
mod interrupt;
mod joypad;
mod mmu;
mod memory;
mod screen;
mod sprite;
mod rom;
mod tile;
mod timer;

const FRAME_DELAY: u64 = 1000000000 * gpu::CYCLES_PER_FRAME / cpu::CYCLES_PER_SECOND;

fn main() {
    use audio::{Audio, AudioState};
    use config::Action;
    use cpu::Cpu;
    use gpu::{CYCLES_PER_FRAME, Gpu, GpuState};
    use interrupt::InterruptState;
    use joypad::{Joypad, JoypadState};
    use mmu::Mmu;
    use rom::Rom;
    use screen::Screen;
    use sdl2::event::Event;
    use sdl2::keyboard::Keycode;
    use std::cell::RefCell;
    use std::path::Path;
    use std::rc::Rc;
    use std::thread;
    use std::time::{Duration};
    use timer::{Timer, TimerState};

    let mut log_control = logger::init().unwrap();

    let config = match config::parse() {
        Action::Run(config) => config,
        Action::ShowUsage(usage) => {
            println!("{}", usage);
            return;
        },
        Action::ShowError(err) => {
            println!("{}", err);
            println!("Use -h or --help to display usage information.");
            return;
        }
    };

    let rom = Rom::new(Path::new(&config.path));

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let audio_subsystem = sdl_context.audio().unwrap();

    let screen = Screen::new(&video_subsystem, config.scale);

    let audio_state = Rc::new(RefCell::new(AudioState::new()));
    let gpu_state = Rc::new(RefCell::new(GpuState::new()));
    let timer_state = Rc::new(RefCell::new(TimerState::new()));
    let joypad_state = Rc::new(RefCell::new(JoypadState::new()));
    let interrupt_state = Rc::new(RefCell::new(InterruptState::new()));

    let mmu = Mmu::new(
        rom,
        audio_state.clone(),
        gpu_state.clone(),
        timer_state.clone(),
        joypad_state.clone(),
        interrupt_state.clone()
    );

    let mut cpu = Cpu::new(mmu, interrupt_state.clone());

    let mut audio = Audio::new(&audio_subsystem, audio_state.clone());

    let mut gpu = Gpu::new(screen, gpu_state.clone(), interrupt_state.clone());

    let mut joypad = Joypad::new(joypad_state.clone(), interrupt_state.clone());

    let mut timer = Timer::new(timer_state.clone(), interrupt_state.clone());

    let mut event_pump = sdl_context.event_pump().unwrap();

    let mut ticks = time::precise_time_ns();

    audio.start();

    'outer: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::KeyDown { keycode: Some(keycode), .. } => {
                    joypad.key_down(keycode);

                    if keycode == Keycode::T {
                        log_control.toggle_trace_mode();
                    }
                },
                Event::KeyUp { keycode: Some(keycode), .. } => {
                    joypad.key_up(keycode);
                },
                Event::Quit { .. } => break 'outer,
                _ => ()
            }
        }

        let mut frame_cycles = 0;

        while frame_cycles < CYCLES_PER_FRAME {
            let delta = cpu.step();
            gpu.step(delta);
            audio.step(delta);
            timer.step(delta);
            frame_cycles += delta;
        }

        // TODO: What should be done if expected_ticks wraps?
        let expected_ticks = ticks.wrapping_add(FRAME_DELAY);
        ticks = time::precise_time_ns();

        if ticks < expected_ticks {
            let delta = expected_ticks - ticks;
            thread::sleep(Duration::new(0, delta as u32));
            ticks += delta;
        }
    }
}
