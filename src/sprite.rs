use memory::Byte;
use std::vec::IntoIter;

const TABLE_SIZE: usize = 40;
const MAX_SPRITES_PER_LINE: usize = 10;

pub struct SpriteTable {
    sprites: [Sprite; TABLE_SIZE]
}

#[derive(Clone, Copy, Default)]
pub struct Sprite {
    y_pos: Byte,
    x_pos: Byte,
    tile: Byte,
    flags: SpriteFlags
}

pub enum SpritePalette {
    Palette0,
    Palette1
}

bitflags! {
    #[derive(Default)]
    flags SpriteFlags: u8 {
        const SPRITE_PRIORITY = 0x80,
        const SPRITE_FLIP_Y = 0x40,
        const SPRITE_FLIP_X = 0x20,
        const SPRITE_PALETTE = 0x10
    }
}

impl SpriteTable {
    pub fn new() -> SpriteTable {
        SpriteTable {
            sprites: [Sprite::default(); TABLE_SIZE]
        }
    }

    pub fn iter_for_line<'a>(&'a self, line: Byte, height: Byte) -> IntoIter<&'a Sprite> {
        let mut sprite_refs: Vec<&Sprite> = self.sprites.iter()
            .filter(|sprite| {
                line >= sprite.y() && line < sprite.y().wrapping_add(height)
            })
            .take(MAX_SPRITES_PER_LINE)
            .collect();

        sprite_refs.sort_by_key(|sprite| sprite.x());

        sprite_refs.into_iter()
    }
}

impl Sprite {
    pub fn x(&self) -> Byte {
        self.x_pos.wrapping_sub(8)
    }

    pub fn y(&self) -> Byte {
        self.y_pos.wrapping_sub(16)
    }

    pub fn tile(&self) -> Byte {
        self.tile
    }

    pub fn low_priority(&self) -> bool {
        self.flags.contains(SPRITE_PRIORITY)
    }

    pub fn flip_x(&self) -> bool {
        self.flags.contains(SPRITE_FLIP_X)
    }

    pub fn flip_y(&self) -> bool {
        self.flags.contains(SPRITE_FLIP_Y)
    }

    pub fn palette(&self) -> SpritePalette {
        match self.flags.contains(SPRITE_PALETTE) {
            false => SpritePalette::Palette0,
            true => SpritePalette::Palette1
        }
    }
}
