use memory::Byte;
use sdl2::pixels::{Color, PixelFormatEnum};
use sdl2::rect::Rect;
use sdl2::render::{Renderer, Texture, TextureAccess};
use sdl2::VideoSubsystem;
use sdl2;
use sdl2_sys::render::{SDL_LockTexture, SDL_UnlockTexture};
use std::mem;
use std::ptr;

pub const DISPLAY_WIDTH: u8 = 160;
pub const DISPLAY_HEIGHT: u8 = 144;

const TEXTURE_WIDTH: u32 = 256;
const TEXTURE_HEIGHT: u32 = 256;

const BYTES_PER_PIXEL: usize = 4;

pub struct Screen {
    renderer: Renderer<'static>,
    texture: Texture,
    pixels: *mut u8,
    row_offset: *mut u8,
    column_offset: *mut u8,
    row_length: usize
}

impl Screen {
    pub fn new(video_subsystem: &VideoSubsystem, scale: u32) -> Screen {
        let window = video_subsystem
            .window(
                "YAR-GBE",
                DISPLAY_WIDTH as u32 * scale,
                DISPLAY_HEIGHT as u32 * scale
            )
            .position_centered()
            .build()
            .unwrap();

        let mut renderer = window.renderer()
            .accelerated()
            .build()
            .unwrap();

        renderer.set_draw_color(Color::RGB(0xFF, 0xFF, 0xFF));
        renderer.clear();

        let texture = renderer.create_texture(
            PixelFormatEnum::ARGB8888,
            TextureAccess::Streaming,
            TEXTURE_WIDTH,
            TEXTURE_HEIGHT
        ).unwrap();

        Screen {
            renderer: renderer,
            texture: texture,
            pixels: ptr::null_mut(),
            row_offset: ptr::null_mut(),
            column_offset: ptr::null_mut(),
            row_length: 0
        }
    }

    pub fn clear(&mut self) {
        self.renderer.clear();
        self.renderer.present();
    }
    
    pub fn begin_frame(&mut self) {
        self.renderer.clear();

        let mut row_length = 0;

        let error_code = unsafe {
            SDL_LockTexture(
                self.texture.raw(),
                ptr::null(),
                mem::transmute(&mut self.pixels),
                &mut row_length
            )
        };

        if error_code != 0 {
            panic!(sdl2::get_error());
        }

        self.row_offset = self.pixels;
        self.column_offset = self.pixels;
        self.row_length = row_length as usize;
    }

    pub fn end_frame(&mut self) {
        unsafe {
            SDL_UnlockTexture(self.texture.raw());
            self.pixels = ptr::null_mut();
            self.row_offset = ptr::null_mut();
            self.column_offset = ptr::null_mut();
        }

        let target_rect = Rect::new(0, 0, DISPLAY_WIDTH as u32, DISPLAY_HEIGHT as u32);

        self.renderer.copy(&self.texture, Some(target_rect), None).unwrap();

        self.renderer.present();
    }

    pub fn set_row(&mut self, row: usize) {
        unsafe { 
            self.row_offset = self.pixels.offset((row * self.row_length) as isize);
            self.column_offset = self.row_offset;
        }
    }

    pub fn set_column(&mut self, column: usize) {
        unsafe {
            self.column_offset = self.row_offset.offset((column * BYTES_PER_PIXEL) as isize);
        }
    }

    pub fn draw_pixel(&mut self, palette: Byte, colour_index: Byte) {
        let internal_colour = match colour_index {
            0x00 => palette & 0x03,
            0x01 => (palette & 0x0C) >> 2,
            0x02 => (palette & 0x30) >> 4,
            0x03 => (palette & 0xC0) >> 6,
            _ => unreachable!()
        };

        let real_colour = match internal_colour {
            0x00 => 0xFF,
            0x01 => 0xC0,
            0x02 => 0x60,
            0x03 => 0x00,
            _ => unreachable!()
        };

        unsafe {
            *self.column_offset = real_colour;
            self.column_offset = self.column_offset.offset(1);
            *self.column_offset = real_colour;
            self.column_offset = self.column_offset.offset(1);
            *self.column_offset = real_colour;
            self.column_offset = self.column_offset.offset(1);
            *self.column_offset = 0xFF; // Alpha channel
            self.column_offset = self.column_offset.offset(1);
        }
    }

    pub fn skip_pixel(&mut self) {
        unsafe {
            self.column_offset = self.column_offset.offset(BYTES_PER_PIXEL as isize);
        }
    }
}
