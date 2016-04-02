use memory::Byte;
use sdl2::pixels::PixelFormatEnum;
use sdl2::rect::Rect;
use sdl2::render::{Renderer, Texture, TextureAccess};
use sdl2::VideoSubsystem;
use std::mem;

pub const DISPLAY_WIDTH: Byte = 160;
pub const DISPLAY_HEIGHT: Byte = 144;

const TEXTURE_WIDTH: usize = 256;
const TEXTURE_HEIGHT: usize = 256;

pub struct Screen {
    renderer: Renderer<'static>,
    texture: Texture,
    pixels: [u32; TEXTURE_WIDTH * TEXTURE_HEIGHT],
    line_offset: usize
}

impl Screen {
    pub fn new(video_subsystem: &VideoSubsystem) -> Screen {
        let window = video_subsystem
            .window(
                "YAR-GBE",
                DISPLAY_WIDTH as u32,
                DISPLAY_HEIGHT as u32
            )
            .position_centered()
            .build()
            .unwrap();

        let renderer = window.renderer()
            .accelerated()
            .build()
            .unwrap();

        let texture = renderer.create_texture(
            PixelFormatEnum::ARGB8888,
            TextureAccess::Streaming,
            TEXTURE_WIDTH as u32,
            TEXTURE_HEIGHT as u32
        ).unwrap();

        Screen {
            renderer: renderer,
            texture: texture,
            pixels: [0; TEXTURE_WIDTH * TEXTURE_HEIGHT],
            line_offset: 0
        }
    }
    
    pub fn set_line(&mut self, line: usize) {
        self.line_offset = line * TEXTURE_WIDTH;
    }

    pub fn draw_pixel(&mut self, line_x: usize, palette: Byte, colour_index: Byte) {
        let colour = match colour_index {
            0x00 => palette & 0x03,
            0x01 => (palette & 0x0C) >> 2,
            0x02 => (palette & 0x30) >> 4,
            0x03 => (palette & 0xC0) >> 6,
            _ => unreachable!()
        };

        unsafe {
            *self.pixels.get_unchecked_mut(self.line_offset + line_x) = match colour {
                0x00 => 0xFFFFFFFF,
                0x01 => 0xFFC0C0C0,
                0x02 => 0xFF606060,
                0x03 => 0xFF000000,
                _ => unreachable!()
            };
        }
    }

    pub fn update(&mut self) {
        let pixel_ref: &[u8] = unsafe { mem::transmute(&self.pixels[..]) };

        self.texture.update(None, pixel_ref, TEXTURE_WIDTH * 4).unwrap();

        self.renderer.copy(
            &self.texture,
            Some(Rect::new(0, 0, DISPLAY_WIDTH as u32, DISPLAY_HEIGHT as u32)),
            None
        );

        self.renderer.present();
    }
}
