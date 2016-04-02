use memory::Byte;

pub const TILE_SIZE: usize = 8;
pub const TILE_MAP_WIDTH: usize = 32;

const TILE_TABLE_SIZE: usize = 384;
const TILE_MAP_HEIGHT: usize = 32;

pub struct TileDataTable {
    tiles: [Tile; TILE_TABLE_SIZE]
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum TileDataRegion {
    Lower,
    Upper
}

#[derive(Clone, Copy, Default)]
pub struct Tile {
    rows: [(Byte, Byte); TILE_SIZE]
}

pub struct TileMap {
    tiles: [[Byte; TILE_MAP_WIDTH]; TILE_MAP_HEIGHT]
}

impl TileDataTable {
    pub fn new() -> TileDataTable {
        TileDataTable {
            tiles: [Tile::default(); TILE_TABLE_SIZE]
        }
    }

    pub fn get<'a>(&'a self, tile_id: Byte, region: TileDataRegion) -> &'a Tile {
        let tile_offset = if tile_id < 128 && region == TileDataRegion::Upper {
            256 + tile_id as usize
        } else {
            tile_id as usize
        };

        &self.tiles[tile_offset]
    }
}

impl Tile {
    pub fn get(&self, x: usize, y: usize) -> Byte {
        let (lower, upper) = self.rows[y];

        let column_bit = 0x80 >> x;

        let lower_bit = (lower & column_bit) != 0;
        let upper_bit = (upper & column_bit) != 0;

        ((upper_bit as Byte) << 1) | (lower_bit as Byte)
    }
}

impl TileMap {
    pub fn new() -> TileMap {
        TileMap {
            tiles: [[0; TILE_MAP_WIDTH]; TILE_MAP_HEIGHT]
        }
    }

    pub fn get(&self, x: usize, y: usize) -> Byte {
        self.tiles[y][x]
    }
}
