use cpu::CYCLES_PER_SECOND;
use memory::Byte;
use sdl2::audio::{AudioCallback, AudioDevice, AudioSpecDesired};
use sdl2::AudioSubsystem;
use std::cell::RefCell;
use std::ops::Add;
use std::rc::Rc;
use std::sync::mpsc::{self, Receiver, Sender};

const SAMPLE_RATE: u64 = 44100;
const SAMPLE_PERIOD: f32 = CYCLES_PER_SECOND as f32 / SAMPLE_RATE as f32;

const FRAME_SEQUENCER_THRESHOLD: u64 = 2048;

const MAX_FREQUENCY: u16 = 2047;
const MAX_PERIOD: u64 = 2048;

pub struct Audio {
    device: AudioDevice<AudioSystemCallback>,
    audio_state: Rc<RefCell<AudioState>>,
    sampler_clock: f32,
    frame_sequencer: FrameSequencer,
    audio_buffer: Sender<Sample>
}

pub struct AudioState {
    enabled: bool,
    left_volume: Byte,
    right_volume: Byte,
    pulse1: PulseChannel,
    pulse2: PulseChannel,
    wave: WaveChannel,
    noise: NoiseChannel
}

struct AudioSystemCallback {
    audio_buffer: Receiver<Sample>
}

struct FrameSequencer {
    clock: u64,
    current_frame: Frame
}

type Frame = u32;

struct PulseChannel {
    enabled: bool,
    frequency_timer: FrequencyTimer,
    frequency_sweep: Option<FrequencySweep>,
    duty_cycle: DutyCycle,
    length_counter: LengthCounter,
    envelope: Envelope,
    mixer: Mixer
}

struct WaveChannel {
    power: bool,
    enabled: bool,
    frequency_timer: FrequencyTimer,
    wave_ram: WaveRam,
    length_counter: LengthCounter,
    volume_control: VolumeControl,
    mixer: Mixer
}

struct NoiseChannel {
    enabled: bool,
    noise_generator: NoiseGenerator,
    length_counter: LengthCounter,
    envelope: Envelope,
    mixer: Mixer
}

struct FrequencyTimer {
    frequency: u16,
    period: u64,
    clock: u64
}

struct FrequencySweep {
    enabled: bool,
    frequency: u16,
    period: u64,
    clock: u64,
    shift: u16,
    negate: bool
}

struct DutyCycle {
    pulse_duty: PulseDuty,
    clock: Byte
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum PulseDuty {
    Duty0 = 0b00000001,
    Duty1 = 0b10000001,
    Duty2 = 0b10000111,
    Duty3 = 0b01111110
}

struct WaveRam {
    ram: [Byte; 16],
    sample: Byte,
    clock: Byte
}

struct NoiseGenerator {
    clock_shift: Byte,
    width_mode: Byte,
    divisor_code: Byte,
    lfsr: u16,
    period: u64,
    clock: u64
}

struct LengthCounter {
    enabled: bool,
    max_value: u32,
    counter: u32
}

struct Envelope {
    starting_volume: Byte,
    current_volume: Byte,
    add: bool,
    period: Byte,
    clock: Byte
}

struct VolumeControl {
    volume: Byte
}

struct Mixer {
    left_enable: bool,
    right_enable: bool
}

struct Sample {
    left: f32,
    right: f32
}

impl Audio {
    pub fn new(
        audio_subsystem: &AudioSubsystem,
        audio_state: Rc<RefCell<AudioState>>
    ) -> Audio {
        let (sender, receiver) = mpsc::channel();

        let desired_spec = AudioSpecDesired {
            freq: Some(SAMPLE_RATE as i32),
            channels: Some(2),
            samples: None
        };

        let device = audio_subsystem.open_playback(None, &desired_spec, move |_spec| {
            AudioSystemCallback::new(receiver)
        }).unwrap();

        Audio {
            device: device,
            audio_state: audio_state,
            sampler_clock: 0.0,
            frame_sequencer: FrameSequencer::new(),
            audio_buffer: sender
        }
    }

    pub fn start(&mut self) {
        self.device.resume();
    }

    pub fn step(&mut self, ticks: u64) {
        let mut audio_state = self.audio_state.borrow_mut();

        // TODO: More precise timing, rather than moving in chunks
        self.sampler_clock += ticks as f32;

        while self.sampler_clock >= SAMPLE_PERIOD {
            self.sampler_clock -= SAMPLE_PERIOD;

            let sample = match audio_state.enabled {
                true => {
                    // Take a sample from each channel. This must be a voltage pair.
                    let sample = audio_state.pulse1.sample() +
                        audio_state.pulse2.sample() +
                        audio_state.wave.sample() +
                        audio_state.noise.sample();

                    Sample::new(
                        sample.left * (audio_state.left_volume + 1) as f32 / 32.0,
                        sample.right * (audio_state.right_volume + 1) as f32 / 32.0
                    )
                },
                false => Sample::new(0.0, 0.0)
            };

            self.audio_buffer.send(sample).unwrap();
        }

        let new_frame = self.frame_sequencer.tick(ticks);

        if audio_state.enabled {
            audio_state.pulse1.tick(ticks);
            audio_state.pulse2.tick(ticks);
            audio_state.wave.tick(ticks);
            audio_state.noise.tick(ticks);

            if new_frame {
                let frame = self.frame_sequencer.frame();
                audio_state.pulse1.on_frame_update(frame);
                audio_state.pulse2.on_frame_update(frame);
                audio_state.wave.on_frame_update(frame);
                audio_state.noise.on_frame_update(frame);
            }
        }
    }
}

impl AudioState {
    pub fn new() -> AudioState {
        AudioState {
            enabled: false,
            left_volume: 0,
            right_volume: 0,
            pulse1: PulseChannel::new(Some(FrequencySweep::new())),
            pulse2: PulseChannel::new(None),
            wave: WaveChannel::new(),
            noise: NoiseChannel::new()
        }
    }

    pub fn byte(&self, offset: usize) -> Byte {
        match offset {
            0x10 ... 0x14 => {
                self.pulse1.byte(offset - 0x10)
            },
            0x15 ... 0x19 => {
                self.pulse2.byte(offset - 0x15)
            },
            0x1A ... 0x1E => {
                self.wave.byte(offset - 0x1A)
            },
            0x1F ... 0x23 => {
                self.noise.byte(offset - 0x1F)
            },
            0x24 => {
                (self.left_volume << 4) + self.right_volume
            },
            0x25 => {
                self.pulse1.mixer.left_enable as Byte * 0x10 +
                self.pulse1.mixer.right_enable as Byte * 0x01 +
                self.pulse2.mixer.left_enable as Byte * 0x20 +
                self.pulse2.mixer.right_enable as Byte * 0x02 +
                self.wave.mixer.left_enable as Byte * 0x40 +
                self.wave.mixer.right_enable as Byte * 0x04 +
                self.noise.mixer.left_enable as Byte * 0x80 +
                self.noise.mixer.right_enable as Byte * 0x08
            },
            0x26 => {
                self.enabled as Byte * 0x80 +
                self.pulse1.enabled as Byte * 0x01 +
                self.pulse2.enabled as Byte * 0x02 +
                self.wave.enabled as Byte * 0x04 +
                self.noise.enabled as Byte * 0x08
            },
            0x30 ... 0x3F => {
                // Always returns the currently selected sample
                self.wave.wave_ram.sample
            },
            _ => {
                // Ignore for now
                0
            }
        }
    }

    pub fn put_byte(&mut self, offset: usize, value: Byte) {
        match offset {
            0x10 ... 0x14 => {
                self.pulse1.put_byte(offset - 0x10, value);
            },
            0x15 ... 0x19 => {
                self.pulse2.put_byte(offset - 0x15, value);
            },
            0x1A ... 0x1E => {
                self.wave.put_byte(offset - 0x1A, value);
            },
            0x1F ... 0x23 => {
                self.noise.put_byte(offset - 0x1F, value);
            },
            0x24 => {
                self.left_volume = (value & 0x70) >> 4;
                self.right_volume = value & 0x07;
            },
            0x25 => {
                self.pulse1.mixer.left_enable = (value & 0x10) != 0;
                self.pulse1.mixer.right_enable = (value & 0x01) != 0;
                self.pulse2.mixer.left_enable = (value & 0x20) != 0;
                self.pulse2.mixer.right_enable = (value & 0x02) != 0;
                self.wave.mixer.left_enable = (value & 0x40) != 0;
                self.wave.mixer.right_enable = (value & 0x04) != 0;
                self.noise.mixer.left_enable = (value & 0x80) != 0;
                self.noise.mixer.right_enable = (value & 0x08) != 0;
            },
            0x26 => {
                self.enabled = (value & 0x80) != 0;

                if !self.enabled {
                    // Disabling the audio system will essentially write 0 to all other registers
                    // Note: Last entry in range below is exclusive!
                    for register in 0x10..0x26 {
                        self.put_byte(register, 0);
                    }
                }
            },
            0x30 ... 0x3F => {
                self.wave.wave_ram.ram[offset - 0x30] = value;
            },
            _ => {
                // Ignore for now
            }
        }
    }
}

impl AudioSystemCallback {
    fn new(audio_buffer: Receiver<Sample>) -> AudioSystemCallback {
        AudioSystemCallback {
            audio_buffer: audio_buffer
        }
    }
}

impl AudioCallback for AudioSystemCallback {
    type Channel = f32;

    fn callback(&mut self, out: &mut [f32]) {
        let mut it = out.iter_mut();

        while let Some(left) = it.next() {
            let sample = match self.audio_buffer.try_recv() {
                Ok(sample) => sample,
                _ => Sample::new(0.0, 0.0)
            };

            *left = sample.left;

            // TODO: Odd numbers shouldn't happen, right?
            *it.next().unwrap() = sample.right;
        }
    }
}

impl FrameSequencer {
    fn new() -> FrameSequencer {
        FrameSequencer {
            clock: 0,
            current_frame: 0
        }
    }

    fn frame(&self) -> Frame {
        self.current_frame
    }

    fn tick(&mut self, ticks: u64) -> bool {
        self.clock += ticks;

        if self.clock >= FRAME_SEQUENCER_THRESHOLD {
            self.clock -= FRAME_SEQUENCER_THRESHOLD;

            self.current_frame += 1;

            if self.current_frame >= 8 {
                self.current_frame = 0;
            }

            true

        } else {
            false
        }
    }
}

impl PulseChannel {
    fn new(frequency_sweep: Option<FrequencySweep>) -> PulseChannel {
        PulseChannel {
            enabled: false,
            frequency_timer: FrequencyTimer::new(),
            frequency_sweep: frequency_sweep,
            duty_cycle: DutyCycle::new(),
            length_counter: LengthCounter::new(64),
            envelope: Envelope::new(),
            mixer: Mixer::new()
        }
    }

    fn byte(&self, offset: usize) -> Byte {
        match offset {
            0 => {
                match self.frequency_sweep {
                    Some(ref frequency_sweep) => frequency_sweep.bits(),
                    None => 0
                }
            },
            1 => {
                (self.duty_cycle.bits() << 6) + self.length_counter.bits()
            },
            2 => {
                self.envelope.bits()
            },
            3 => {
                self.frequency_timer.lower_bits()
            },
            4 => {
                self.frequency_timer.upper_bits() +
                    (self.length_counter.enabled as Byte & 0x40)
            },
            _ => unreachable!()
        }
    }

    fn put_byte(&mut self, offset: usize, value: Byte) {
        match offset {
            0 => {
                if let Some(ref mut frequency_sweep) = self.frequency_sweep {
                    frequency_sweep.set_bits(value);
                }
            },
            1 => {
                self.duty_cycle.set_bits((value & 0xC0) >> 6);
                self.length_counter.set_bits(value & 0x3F);
            },
            2 => {
                self.envelope.set_bits(value);
            },
            3 => {
                self.frequency_timer.set_lower_bits(value);
            },
            4 => {
                self.frequency_timer.set_upper_bits(value & 0x07);
                self.length_counter.enabled = (value & 0x40) != 0;

                if (value & 0x80) != 0 {
                    self.enabled = true;
                    self.frequency_timer.reset();
                    self.length_counter.reset();
                    self.envelope.reset();

                    if let Some(ref mut frequency_sweep) = self.frequency_sweep {
                        if frequency_sweep.reset(&mut self.frequency_timer) {
                            self.enabled = false;
                        }
                    }
                }
            },
            _ => unreachable!()
        }
    }

    fn tick(&mut self, ticks: u64) {
        if self.frequency_timer.tick(ticks) {
            self.duty_cycle.advance();
        }
    }

    fn on_frame_update(&mut self, frame: Frame) {
        if self.length_counter.on_frame_update(frame) {
            self.enabled = false;
        }

        if let Some(ref mut frequency_sweep) = self.frequency_sweep {
            if frequency_sweep.on_frame_update(frame, &mut self.frequency_timer) {
                self.enabled = false;
            }
        }

        self.envelope.on_frame_update(frame);
    }

    fn sample(&self) -> Sample {
        if self.enabled {
            Sample::from(self.mixer.process(self.envelope.process(self.duty_cycle.sample())))
        } else {
            Sample::new(0.0, 0.0)
        }
    }
}

impl WaveChannel {
    fn new() -> WaveChannel {
        WaveChannel {
            power: false,
            enabled: false,
            frequency_timer: FrequencyTimer::new(),
            wave_ram: WaveRam::new(),
            length_counter: LengthCounter::new(256),
            volume_control: VolumeControl::new(),
            mixer: Mixer::new()
        }
    }

    fn byte(&self, offset: usize) -> Byte {
        match offset {
            0 => self.power as Byte * 0x80,
            1 => self.length_counter.bits(),
            2 => self.volume_control.bits() << 5,
            3 => self.frequency_timer.lower_bits(),
            4 => {
                self.frequency_timer.upper_bits() +
                    (self.length_counter.enabled as Byte & 0x40)
            },
            _ => unreachable!()
        }
    }

    fn put_byte(&mut self, offset: usize, value: Byte) {
        match offset {
            0 => self.power = (value & 0x80) != 0,
            1 => self.length_counter.set_bits(value),
            2 => self.volume_control.set_bits((value & 0x60) >> 5),
            3 => self.frequency_timer.set_lower_bits(value),
            4 => {
                self.frequency_timer.set_upper_bits(value & 0x07);
                self.length_counter.enabled = (value & 0x40) != 0;

                if (value & 0x80) != 0 {
                    self.enabled = true;
                    self.frequency_timer.reset();
                    self.wave_ram.reset();
                    self.length_counter.reset();
                }
            },
            _ => unreachable!()
        }
    }

    fn tick(&mut self, ticks: u64) {
        if !self.power {
            return;
        }

        // We cycle through our wave RAM at twice the rate the pulse channels
        // go through their duty cycle
        if self.frequency_timer.tick(ticks * 2) {
            self.wave_ram.advance();
        }
    }

    fn on_frame_update(&mut self, frame: Frame) {
        if !self.power {
            return;
        }

        if self.length_counter.on_frame_update(frame) {
            self.enabled = false;
        }
    }

    fn sample(&self) -> Sample {
        if self.power && self.enabled {
            Sample::from(
                self.mixer.process(
                    self.volume_control.process(
                        self.wave_ram.sample()
                    )
                )
            )
        } else {
            Sample::new(0.0, 0.0)
        }
    }
}

impl NoiseChannel {
    fn new() -> NoiseChannel {
        NoiseChannel {
            enabled: false,
            noise_generator: NoiseGenerator::new(),
            length_counter: LengthCounter::new(64),
            envelope: Envelope::new(),
            mixer: Mixer::new()
        }
    }

    fn byte(&self, offset: usize) -> Byte {
        match offset {
            0 => 0, // Not used
            1 => self.length_counter.bits(),
            2 => self.envelope.bits(),
            3 => self.noise_generator.bits(),
            4 => self.length_counter.enabled as Byte & 0x40,
            _ => unreachable!()
        }
    }

    fn put_byte(&mut self, offset: usize, value: Byte) {
        match offset {
            0 => (), // Not used
            1 => self.length_counter.set_bits(value & 0x3F),
            2 => self.envelope.set_bits(value),
            3 => self.noise_generator.set_bits(value),
            4 => {
                self.length_counter.enabled = (value & 0x40) != 0;

                if (value & 0x80) != 0 {
                    self.enabled = true;
                    self.length_counter.reset();
                    self.noise_generator.reset();
                    self.envelope.reset();
                }
            },
            _ => unreachable!()
        }
    }

    fn tick(&mut self, ticks: u64) {
        self.noise_generator.tick(ticks);
    }

    fn on_frame_update(&mut self, frame: Frame) {
        if self.length_counter.on_frame_update(frame) {
            self.enabled = false;
        }

        self.envelope.on_frame_update(frame);
    }

    fn sample(&self) -> Sample {
        if self.enabled {
            Sample::from(
                self.mixer.process(
                    self.envelope.process(
                        self.noise_generator.sample()
                    )
                )
            )
        } else {
            Sample::new(0.0, 0.0)
        }
    }
}

impl FrequencyTimer {
    fn new() -> FrequencyTimer {
        FrequencyTimer {
            frequency: 0,
            period: MAX_PERIOD,
            clock: 0
        }
    }

    fn frequency(&self) -> u16 {
        self.frequency
    }

    fn set_frequency(&mut self, frequency: u16) {
        self.frequency = frequency;
    }

    fn lower_bits(&self) -> Byte {
        (self.frequency & 0x00FF) as Byte
    }

    fn set_lower_bits(&mut self, value: Byte) {
        self.frequency = (self.frequency & 0x0700) | (value as u16);
        self.period = MAX_PERIOD - self.frequency as u64
    }

    fn upper_bits(&self) -> Byte {
        ((self.frequency & 0x0700) >> 8) as Byte
    }

    fn set_upper_bits(&mut self, value: Byte) {
        self.frequency = (((value as u16) << 8) & 0x0700) | (self.frequency & 0x00FF);
        self.period = MAX_PERIOD - self.frequency as u64
    }

    fn reset(&mut self) {
        self.clock = 0;
    }

    fn tick(&mut self, ticks: u64) -> bool {
        self.clock += ticks;

        if self.clock >= self.period {
            self.clock -= self.period;
            true
        } else {
            false
        }
    }
}

impl FrequencySweep {
    fn new() -> FrequencySweep {
        FrequencySweep {
            enabled: false,
            frequency: 0,
            period: 0,
            clock: 0,
            shift: 0,
            negate: false
        }
    }

    fn bits(&self) -> Byte {
        ((self.period as Byte) << 4) + ((self.negate as Byte) << 3) + self.shift as Byte
    }

    fn set_bits(&mut self, value: Byte) {
        self.period = ((value & 0x70) >> 4) as u64;
        self.negate = (value & 0x08) != 0;
        self.shift = (value & 0x07) as u16;
    }

    fn reset(&mut self, frequency_timer: &mut FrequencyTimer) -> bool {
        self.frequency = frequency_timer.frequency();
        self.clock = 0;
        self.enabled = self.period != 0 || self.shift != 0;

        if self.shift != 0 {
            self.sweep(frequency_timer)
        } else {
            false
        }
    }

    fn on_frame_update(&mut self, frame: Frame, frequency_timer: &mut FrequencyTimer) -> bool {
        if self.enabled && frame == 7 {
            self.clock += 1;

            if self.clock >= self.period {
                self.clock = 0;
                return self.sweep(frequency_timer)
            }
        }

        false
    }
    
    fn sweep(&mut self, frequency_timer: &mut FrequencyTimer) -> bool {
        let delta = self.frequency >> self.shift;

        if self.negate {
            self.frequency = self.frequency.wrapping_sub(delta);
        } else {
            self.frequency = self.frequency.wrapping_add(delta);
        };

        let overflow = self.frequency > MAX_FREQUENCY;

        self.frequency &= MAX_FREQUENCY;

        if !overflow {
            frequency_timer.set_frequency(self.frequency);
        }

        overflow
    }
}

impl DutyCycle {
    fn new() -> DutyCycle {
        DutyCycle {
            pulse_duty: PulseDuty::Duty0,
            clock: 0
        }
    }

    fn bits(&self) -> Byte {
        match self.pulse_duty {
            PulseDuty::Duty0 => 0x00,
            PulseDuty::Duty1 => 0x01,
            PulseDuty::Duty2 => 0x02,
            PulseDuty::Duty3 => 0x03
        }
    }

    fn set_bits(&mut self, value: Byte) {
        self.pulse_duty = match value {
            0x00 => PulseDuty::Duty0,
            0x01 => PulseDuty::Duty1,
            0x02 => PulseDuty::Duty2,
            0x03 => PulseDuty::Duty3,
            _ => unreachable!()
        };
    }

    fn advance(&mut self) {
        self.clock += 1;

        if self.clock >= 8 {
            self.clock = 0;
        }
    }

    fn sample(&self) -> Byte {
        (self.pulse_duty as Byte & (0b10000000 >> self.clock)) >> (7 - self.clock)
    }
}

impl WaveRam {
    fn new() -> WaveRam {
        WaveRam {
            ram: [0; 16],
            sample: 0,
            clock: 0
        }
    }

    fn reset(&mut self) {
        self.clock = 0;
    }

    fn advance(&mut self) {
        self.clock += 1;

        if self.clock >= 32 {
            self.clock = 0;
        }

        self.sample = self.ram[self.clock as usize / 2];
    }

    fn sample(&self) -> Byte {
        if self.clock % 2 == 0 {
            (self.sample & 0xF0) >> 4
        } else {
            self.sample & 0x0F
        }
    }
}

impl NoiseGenerator {
    fn new() -> NoiseGenerator {
        NoiseGenerator {
            clock_shift: 0,
            width_mode: 0,
            divisor_code: 0,
            lfsr: 0,
            period: 2,
            clock: 0
        }
    }

    fn bits(&self) -> Byte {
        (self.clock_shift << 4) +
            (self.width_mode << 3) +
            self.divisor_code
    }

    fn set_bits(&mut self, value: Byte) {
        self.clock_shift = (value & 0xF0) >> 4;
        self.width_mode = (value & 0x08) >> 3;
        self.divisor_code = value & 0x07;

        let divisor = match self.divisor_code {
            0x00 => 2,
            0x01 => 4,
            0x02 => 8,
            0x03 => 12,
            0x04 => 16,
            0x05 => 20,
            0x06 => 24,
            0x07 => 28,
            _ => unreachable!()
        };

        self.period = divisor << (self.clock_shift as u64);
    }

    fn reset(&mut self) {
        self.lfsr = 0x7FFF;
    }

    fn tick(&mut self, ticks: u64) {
        self.clock += ticks;

        while self.clock >= self.period {
            self.clock -= self.period;

            let xor = ((self.lfsr & 0x0002) >> 1) ^ (self.lfsr & 0x0001);
            self.lfsr = ((self.lfsr & 0x7FFE) >> 1) | (xor << 14);

            if self.width_mode == 1 {
                self.lfsr = (self.lfsr & 0x7FBF) | (xor << 6)
            }
        }
    }

    fn sample(&self) -> Byte {
        ((!self.lfsr) & 0x0001) as Byte
    }
}

impl LengthCounter {
    fn new(max_value: u32) -> LengthCounter {
        LengthCounter {
            enabled: false,
            max_value: max_value,
            counter: 0
        }
    }

    fn bits(&self) -> Byte {
        (self.max_value - self.counter) as Byte
    }

    fn set_bits(&mut self, value: Byte) {
        self.counter = self.max_value - value as u32;
    }

    fn reset(&mut self) {
        if self.counter == 0 {
            self.counter = self.max_value;
        }
    }

    fn on_frame_update(&mut self, frame: Frame) -> bool {
        if !self.enabled {
            return false
        }

        if frame % 2 == 0 && self.counter > 0 {
            self.counter -= 1;
        }

        self.counter == 0
    }
}

impl Envelope {
    fn new() -> Envelope {
        Envelope {
            starting_volume: 0,
            current_volume: 0,
            add: false,
            period: 0,
            clock: 0
        }
    }

    fn bits(&self) -> Byte {
        (self.starting_volume << 4) + ((self.add as Byte) << 3) + self.period
    }
    
    fn set_bits(&mut self, value: Byte) {
        self.starting_volume = (value & 0xF0) >> 4;
        self.add = (value & 0x08) != 0;
        self.period = value & 0x07;
    }

    fn reset(&mut self) {
        self.current_volume = self.starting_volume;
        self.clock = 0;
    }

    fn on_frame_update(&mut self, frame: Frame) {
        if frame != 7 || self.period == 0 {
            return;
        }

        self.clock += 1;

        if self.clock >= self.period {
            self.clock = 0;

            if self.add && self.current_volume < 15 {
                self.current_volume += 1;
            } else if !self.add && self.current_volume > 0 {
                self.current_volume -= 1;
            }
        }
    }

    fn process(&self, input: Byte) -> Byte {
        input * self.current_volume
    }
}

impl VolumeControl {
    fn new() -> VolumeControl {
        VolumeControl {
            volume: 0
        }
    }

    fn bits(&self) -> Byte {
        self.volume
    }

    fn set_bits(&mut self, value: Byte) {
        self.volume = value & 0x03;
    }

    fn process(&self, input: Byte) -> Byte {
        match self.volume {
            0x00 => 0,
            0x01 => input,
            0x02 => (input & 0x0E) >> 1,
            0x03 => (input & 0x0C) >> 2,
            _ => unreachable!()
        }
    }
}

impl Mixer {
    fn new() -> Mixer {
        Mixer {
            left_enable: false,
            right_enable: false
        }
    }

    fn process(&self, input: Byte) -> (Byte, Byte) {
        (input * self.left_enable as Byte, input * self.right_enable as Byte)
    }
}

impl Sample {
    fn new(left: f32, right: f32) -> Sample {
        Sample {
            left: left,
            right: right
        }
    }
}

impl Add for Sample {
    type Output = Sample;

    fn add(self, rhs: Sample) -> Sample {
        Sample {
            left: self.left + rhs.left,
            right: self.right + rhs.right
        }
    }
}

impl From<(Byte, Byte)> for Sample {
    fn from(tuple: (Byte, Byte)) -> Sample {
        Sample {
            left: (tuple.0 as f32 - 7.5) / 7.5,
            right: (tuple.1 as f32 - 7.5) / 7.5
        }
    }
}
