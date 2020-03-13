//has ROM and/or RAM on the cart (through the mapper), and 2Kb in RAM on the console.

//PPU registers
//control bitfield, see the PPUControlFlags enum
const PPUCTRL: usize = 0x2000;
//Masking bitfield, see PPUMask enum
const PPUMASK: usize = 0x2001;
//PPU status, see PPUStatus enum.
//Read resets write pait for 0x2005/0x2006
const PPUSTATUS: usize = 0x2002;
//OAM read/write address
//the address of OAM the we are currently accessing
//the value is set to 0 during each of ticks 257-320 (sprite tile loading interval) of the pre-render and visible scanlines
const OAMADDR: usize = 0x2003;
//OAM data read/write
//write OAM data here, writes will increment OAMADDR after the write
const OAMDATA: usize = 0x2004;
//fine scroll position (two writes, X scroll, Y scroll)
//used to change the scroll position, to tell the PPU which pixel of the nametable should be at the top left corner of the rendered screen.
const PPUSCROLL: usize = 0x2005;
//PPU read/write address (two writes, MSB then LSB)
//the CPU writes to VRAM through a pair of registers on the PPU.
//first an address is loaded into PPUADDR and then it writes repeatedly to PPUDATA to fill VRAM
const PPUADDR: usize = 0x2006;
//PPU data read/write
//after read/write, it will increment PPUADDR by a value determined by bit 2 of PPUCTRL
const PPUDATA: usize = 0x2007;
//OAM DMA high address
//writing a byte 0xXX here will cause a transfer of data from 0xXX00-0xXXFF to the PPU OAM.
//the CPU is disabled during the transfer, about 513 to 514 cycles (1 dummy read cycle, +1 if on an odd cpu cycle, then 256 alternating read/write cycles)
//the transfer will begin at OAMADDR
const OAMDMA: usize = 0x4014;

enum PPUControlFlags {
	//two bits, Nametable select
	//0 = 0x2000, 1 = 0x2400, 2 = 0x2800, 3 = 0x2C00
	NN = 0,
	//VRAM address increment per CPU read/write of PPUDATA
	//0: add 1 (going across a single line), 1: add 32 (going down lines)
	I=2,
	//sprite pattern table address for 8x8 sprites (ignored in 8x16 mode)
	//0: 0x0000, 1: 0x1000
	S=3,
	//Background pattern table address
	//0: 0x0000, 1: 0x1000
	B=4,
	//Sprite Height
	//0: 8x8, 1: 8x16
	H=5,
	//PPU master/slave
	//0: read backgroun from EXT pins; 1: output color on EXT pins
	P=6,
	//NMI enable
	//if on, generate an NMI at the start of the VBLANK interval
	//if the PPU is in VBLANK and this bit is toggled, an NMI is immediately dispatched
	V=7
}

enum PPUMask {
	//Greyscale
	//0: Normal color, 1: produces a greyscale image
	G = 0,
	//Background left column enable
	//1: show background in leftmost 8 pixels of the screen, 0: hide
	m,
	//Sprite left column enable
	//show sprites in the leftmost 8 pixels of the screen, 0: hide
	M,
	//background enable
	//1: show background, 0: hide
	b,
	//sprite enable
	//1: show sprites, 0: hide
	s,
	//Color Emphasis
	emphasize_R,
	emphasize_G,
	emphasize_B,
}

enum PPUStatus {
	//Sprite overflow
	//this flag is set during sprite evaluation and cleared at dot 1 of the pre-render line
	//it is supposed to be set when there are more than eight sprites on a scanline
	O = 5,
	//sprite 0 hit
	//set when a nonzero pixel of sprite 0 overlaps a nonzero background pixel
	//cleared at dot 1 of the pre-render line
	S,
	//vblank
	//0: not in vblank, 1: in vblank
	//set at dot 1 of line 241 (the line after the post-render line), cleared after reading 0x2002 and at dot 1 of the pre-render line
	V
}

pub struct PPU {

}

impl PPU {
	pub fn write_u8(&mut self, address: usize, data: u8){
		match address {
			PPUCTRL => {},
			PPUMASK => {},
			PPUSTATUS => {},
			OAMADDR => {},
			OAMDATA => {},
			PPUSCROLL => {},
			PPUADDR => {},
			PPUDATA => {},
			OAMDMA => {},
			_ => {}
		}
	}

	pub fn read_u8(&self, address: usize) -> u8 {
		match address {
			PPUCTRL => {0},
			PPUMASK => {0},
			PPUSTATUS => {0},
			OAMADDR => {0},
			OAMDATA => {0},
			PPUSCROLL => {0},
			PPUADDR => {0},
			PPUDATA => {0},
			OAMDMA => {0},
			_ => {0}
		}
	}
}