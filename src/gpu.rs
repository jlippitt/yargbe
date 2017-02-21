use interrupt::{Interrupt, InterruptState};
use memory::Byte;
use std::cell::RefCell;
use std::mem;
use std::rc::Rc;
use screen::{DISPLAY_WIDTH, DISPLAY_HEIGHT, Screen};
use sprite::{SpritePalette, SpriteTable};
use tile::{TileDataRegion, TileDataTable, TileMap, TILE_MAP_WIDTH, TILE_SIZE};

pub const CYCLES_PER_FRAME: u64 = 17556;

pub const VRAM_SIZE: usize = 8192;
pub const OAM_SIZE: usize = 160;

pub struct Gpu {
    screen: Screen,
    gpu_state: Rc<RefCell<GpuState>>,
    interrupt_state: Rc<RefCell<InterruptState>>,
    lcd_on: bool,
    mode: GpuMode,
    mode_clock: u64,
    pixel_active: [bool; DISPLAY_WIDTH as usize]
}

pub struct GpuState {
    vram: Vram,
    oam: Oam,
    control: Control,
    lcd_status: Byte,
    scroll_y: Byte,
    scroll_x: Byte,
    line_y: Byte,
    line_y_compare: Byte,
    bg_palette: Byte,
    obj_palette0: Byte,
    obj_palette1: Byte,
    window_y: Byte,
    window_x: Byte
}

struct Vram {
    tile_data: TileDataTable,
    tile_map_0: TileMap,
    tile_map_1: TileMap
}

struct Oam {
    sprite_table: SpriteTable
}

enum TileMapIndex {
    TileMap0,
    TileMap1
}

bitflags! {
    flags Control: u8 {
        const MASTER_DISPLAY = 0x80,
        const WINDOW_TILE_MAP = 0x40,
        const WINDOW_DISPLAY = 0x20,
        const TILE_DATA = 0x10,
        const BG_TILE_MAP = 0x08,
        const SPRITE_SIZE = 0x04,
        const SPRITE_DISPLAY = 0x02,
        const BG_DISPLAY = 0x01
    }
}

enum GpuMode {
    Oam,
    Vram,
    HBlank,
    VBlank
}

impl Gpu {
    pub fn new(
        screen: Screen,
        gpu_state: Rc<RefCell<GpuState>>,
        interrupt_state: Rc<RefCell<InterruptState>>
    ) -> Gpu {
        Gpu {
            screen: screen,
            gpu_state: gpu_state,
            interrupt_state: interrupt_state,
            lcd_on: false,
            mode: GpuMode::HBlank,
            mode_clock: 0,
            pixel_active: [false; DISPLAY_WIDTH as usize]
        }
    }

    pub fn step(&mut self, delta: u64) {
        let control = self.gpu_state.borrow().control;

        // Switch screen on/off (if necessary) according to register value
        if self.lcd_on {
            if !control.contains(MASTER_DISPLAY) {
                // Blank the whole screen until it gets switched on again
                self.screen.clear();
                self.lcd_on = false;
                debug!("Screen OFF");
            }
        } else {
            if control.contains(MASTER_DISPLAY) {
                // Reset the mode clock (to prevent errors)
                self.mode = GpuMode::HBlank;
                self.mode_clock = 0;
                self.gpu_state.borrow_mut().line_y = 0;
                self.screen.begin_frame();
                self.lcd_on = true;
                debug!("Screen ON");
            }
        }

        // If the screen is on, update the internal clock
        if !self.lcd_on {
            return;
        }

        self.mode_clock += delta;

        if self.mode_clock < self.mode.duration() {
            return;
        }

        self.mode_clock -= self.mode.duration();

        // For each mode, tick over to the next mode and/or increment the line
        // count (whichever is applicable)
        match self.mode {
            GpuMode::Oam => {
                self.set_mode(GpuMode::Vram);
            },
            GpuMode::Vram => {
                self.render_line();
                self.set_mode(GpuMode::HBlank);
            },
            GpuMode::HBlank => {
                let line_y = self.next_line();

                if line_y == DISPLAY_HEIGHT {
                    self.screen.end_frame();
                    self.set_mode(GpuMode::VBlank);
                    self.interrupt_state.borrow_mut().fire(Interrupt::VBlank);
                } else {
                    self.set_mode(GpuMode::Oam);
                }
            },
            GpuMode::VBlank => {
                let line_y = self.next_line();

                if line_y == 0 {
                    self.set_mode(GpuMode::Oam);
                    self.screen.begin_frame();
                }
            }
        };
    }

    fn set_mode(&mut self, mode: GpuMode) {
        self.mode = mode;

        let mut gpu_state = self.gpu_state.borrow_mut();

        let mut lcd_status = gpu_state.lcd_status;

        // Set bits 1-0 on the LCD status register
        lcd_status = (lcd_status & 0xFC) + match self.mode {
            GpuMode::Oam => 0x02,
            GpuMode::Vram => 0x03,
            GpuMode::HBlank => 0x00,
            GpuMode::VBlank => 0x01
        };

        // Choose whether to fire an interrupt based on bits 5-3 of the LCD
        // status register
        let interrupt = match self.mode {
            GpuMode::Oam => lcd_status & 0x20 != 0,
            GpuMode::Vram => false,
            GpuMode::HBlank => lcd_status & 0x08 != 0,
            GpuMode::VBlank => lcd_status & 0x10 != 0
        };

        gpu_state.lcd_status = lcd_status;

        if interrupt {
            self.interrupt_state.borrow_mut().fire(Interrupt::LcdStat);
        }
    }

    fn next_line(&mut self) -> Byte {
        let mut gpu_state = self.gpu_state.borrow_mut();

        if gpu_state.line_y < (DISPLAY_HEIGHT + 9) {
            gpu_state.line_y += 1;
        } else {
            gpu_state.line_y = 0;
        }

        if gpu_state.line_y == gpu_state.line_y_compare {
            gpu_state.lcd_status |= 0x04;

            if gpu_state.lcd_status & 0x40 != 0 {
                self.interrupt_state.borrow_mut().fire(Interrupt::LcdStat);
            }

        } else {
            gpu_state.lcd_status &= !0x04;
        }

        gpu_state.line_y
    }

    fn render_line(&mut self) {
        let control = self.gpu_state.borrow().control;
        let line_y = self.gpu_state.borrow().line_y;

        if !control.contains(MASTER_DISPLAY) {
            return;
        }

        self.screen.set_row(line_y as usize);

        // We need to determine if the background or window has draw to a
        // particular pixel so that we can draw low priority sprites behind
        // the background layer. To start with, no one has drawn anywhere, so
        // everything gets set to false!
        self.pixel_active = [false; DISPLAY_WIDTH as usize];

        if control.contains(BG_DISPLAY) {
            let tile_data_region = match control.contains(TILE_DATA) {
                false => TileDataRegion::Upper,
                true => TileDataRegion::Lower
            };

            let tile_map_index = match control.contains(BG_TILE_MAP) {
                false => TileMapIndex::TileMap0,
                true => TileMapIndex::TileMap1
            };

            let scroll_x = self.gpu_state.borrow().scroll_x;
            let scroll_y = self.gpu_state.borrow().scroll_y;

            self.draw_background(
                tile_map_index,
                tile_data_region,
                scroll_x,
                scroll_y,
                0
            );

            if control.contains(WINDOW_DISPLAY) {
                let window_y = self.gpu_state.borrow().window_y;

                // Make sure we've reached the start of the window
                if  line_y >= window_y {
                    let window_x = self.gpu_state.borrow().window_x;
                    
                    let start_x = if window_x > 7 {
                        window_x.wrapping_sub(7)
                    } else {
                        0
                    };

                    let tile_map_index = match control.contains(WINDOW_TILE_MAP) {
                        false => TileMapIndex::TileMap0,
                        true => TileMapIndex::TileMap1
                    };

                    self.draw_background(
                        tile_map_index,
                        tile_data_region,
                        0,
                        0u8.wrapping_sub(window_y),
                        start_x
                    );
                }
            }
        }

        if control.contains(SPRITE_DISPLAY) {
            let gpu_state = self.gpu_state.borrow();

            let sprite_table = &gpu_state.oam.sprite_table;

            let tile_data = &gpu_state.vram.tile_data;

            let tile_size = TILE_SIZE as Byte;

            let sprite_height = match control.contains(SPRITE_SIZE) {
                false => tile_size,
                true => tile_size * 2
            };

            // Draw right to left, as sprites further left appear on top of
            // sprites further right
            for sprite in sprite_table.iter_for_line(line_y, sprite_height).rev() {
                let offset_y = match sprite.flip_y() {
                    false => line_y.wrapping_sub(sprite.y()),
                    true => (sprite_height - 1) - (line_y.wrapping_sub(sprite.y()))
                };

                let tile_id = match control.contains(SPRITE_SIZE) {
                    false => sprite.tile(),
                    true => {
                        let tile_id = sprite.tile() & 0xFE;

                        if offset_y < 8 {
                            tile_id
                        } else {
                            tile_id.wrapping_add(1)
                        }
                    }
                };

                let tile = tile_data.get(tile_id, TileDataRegion::Lower);

                let palette = match sprite.palette() {
                    SpritePalette::Palette0 => gpu_state.obj_palette0,
                    SpritePalette::Palette1 => gpu_state.obj_palette1
                };

                let start_x = sprite.start_x();

                if start_x == 0 {
                    self.screen.set_column(sprite.x() as usize);
                } else {
                    self.screen.set_column(0);
                }

                for pos_x in start_x..tile_size {
                    let line_x = sprite.x().wrapping_add(pos_x);

                    // Don't draw the pixel if it's not on the screen
                    if line_x >= DISPLAY_WIDTH {
                        self.screen.skip_pixel();
                        continue;
                    }

                    // Don't draw the sprite if it's low priority and the
                    // background layer (including window) has draw something
                    // other than colour 0 onto this pixel.
                    if sprite.low_priority() && self.pixel_active[line_x as usize] {
                        self.screen.skip_pixel();
                        continue;
                    }

                    let offset_x = match sprite.flip_x() {
                        false => pos_x,
                        true => (tile_size - 1) - pos_x
                    };

                    let colour_index = tile.get(
                        offset_x as usize,
                        (offset_y % 8) as usize
                    );

                    if colour_index == 0 {
                        // Pixel is transparent
                        self.screen.skip_pixel();
                        continue;
                    }

                    self.screen.draw_pixel(palette, colour_index);
                }
            }
        }
    }

    fn draw_background(
        &mut self,
        tile_map_index: TileMapIndex,
        tile_data_region: TileDataRegion,
        scroll_x: Byte,
        scroll_y: Byte,
        start_x: Byte
    ) {
        let gpu_state = self.gpu_state.borrow();

        let tile_data = &gpu_state.vram.tile_data;

        let tile_map = match tile_map_index {
            TileMapIndex::TileMap0 => &gpu_state.vram.tile_map_0,
            TileMapIndex::TileMap1 => &gpu_state.vram.tile_map_1
        };

        let pos_x = scroll_x as usize;
        let pos_y = scroll_y.wrapping_add(gpu_state.line_y) as usize;

        let mut index_x = pos_x / TILE_SIZE;
        let index_y = pos_y / TILE_SIZE;

        let mut offset_x = pos_x % TILE_SIZE;
        let offset_y = pos_y % TILE_SIZE;

        // Look up the initial tile
        let mut tile_id = tile_map.get(index_x, index_y);
        let mut tile = tile_data.get(tile_id, tile_data_region);

        self.screen.set_column(start_x as usize);

        for line_x in start_x..DISPLAY_WIDTH {
            let colour_index = tile.get(offset_x, offset_y);

            if colour_index > 0 {
                // Mark this pixel as 'active' so we know later on that it
                // should hide low priority sprites.
                self.pixel_active[line_x as usize] = true;
            }

            self.screen.draw_pixel(gpu_state.bg_palette, colour_index);

            offset_x += 1;

            if offset_x == TILE_SIZE {
                offset_x = 0;

                index_x += 1;

                if index_x == TILE_MAP_WIDTH {
                    index_x = 0;
                }

                tile_id = tile_map.get(index_x, index_y);
                tile = tile_data.get(tile_id, tile_data_region);
            }
        }
    }
}

impl GpuState {
    pub fn new() -> GpuState {
        GpuState {
            vram: Vram {
                tile_data: TileDataTable::new(),
                tile_map_0: TileMap::new(),
                tile_map_1: TileMap::new()
            },
            oam: Oam {
                sprite_table: SpriteTable::new()
            },
            control: Control::empty(),
            lcd_status: 0x82,
            scroll_y: 0x00,
            scroll_x: 0x00,
            line_y: 0x00,
            line_y_compare: 0x00,
            bg_palette: 0x00,
            obj_palette0: 0x00,
            obj_palette1: 0x00,
            window_x: 0x00,
            window_y: 0x00
        }
    }

    pub fn vram_byte(&self, offset: usize) -> Byte {
        let vram: &[Byte; VRAM_SIZE] = unsafe { mem::transmute(&self.vram) };
        vram[offset]
    }

    pub fn put_vram_byte(&mut self, offset: usize, value: Byte) {
        let vram: &mut [Byte; VRAM_SIZE] = unsafe { mem::transmute(&mut self.vram) };
        vram[offset] = value;
    }

    pub fn oam_byte(&self, offset: usize) -> Byte {
        let oam: &[Byte; OAM_SIZE] = unsafe { mem::transmute(&self.oam) };
        oam[offset]
    }

    pub fn put_oam_byte(&mut self, offset: usize, value: Byte) {
        let oam: &mut [Byte; OAM_SIZE] = unsafe { mem::transmute(&mut self.oam) };
        oam[offset] = value;
    }

    pub fn register_byte(&self, offset: usize) -> Byte {
        match offset {
            0x00 => self.control.bits(),
            0x01 => self.lcd_status,
            0x02 => self.scroll_y,
            0x03 => self.scroll_x,
            0x04 => self.line_y,
            0x05 => self.line_y_compare,
            0x07 => self.bg_palette,
            0x08 => self.obj_palette0,
            0x09 => self.obj_palette1,
            0x0A => self.window_y,
            0x0B => self.window_x,
            _ => unreachable!()
        }
    }

    pub fn put_register_byte(&mut self, offset: usize, value: Byte) {
        match offset {
            0x00 => {
                debug!("GPU control: {:02X}", value);
                self.control = Control::from_bits_truncate(value);
            },
            0x01 => {
                // Only bits 6-3 can be set. Bits 2-0 must be preserved.
                let old_status = self.lcd_status;
                self.lcd_status = 0x80 | (value & 0x78) | (old_status & 0x07);
            },
            0x02 => self.scroll_y = value,
            0x03 => self.scroll_x = value,
            0x04 => (),
            0x05 => self.line_y_compare = value,
            0x07 => self.bg_palette = value,
            0x08 => self.obj_palette0 = value,
            0x09 => self.obj_palette1 = value,
            0x0A => self.window_y = value,
            0x0B => self.window_x = value,
            _ => unreachable!()
        };
    }
}

impl GpuMode {
    fn duration(&self) -> u64 {
        match *self {
            GpuMode::Oam => 20,
            GpuMode::Vram => 43,
            GpuMode::HBlank => 51,
            GpuMode::VBlank => 114
        }
    }
}
