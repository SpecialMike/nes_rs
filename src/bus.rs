use crate::Mapper;
use crate::PPU;

//represents the addressable spaces on the NES.
//This is the CPU bus, not the PPU bus
pub struct Bus {
	work_ram: Vec<u8>,
	mapper: Mapper,
	ppu: PPU,
}

impl Bus {
	pub fn new(filename: &str) -> Bus{
		Bus {
			work_ram: vec![0; 0x800],
			mapper: Mapper::new(filename),
			ppu: PPU{}
		}
	}
	pub fn read_u8(&self, address: usize) -> u8 {
		//println!("Reading {:x?} from {:x?}", self.ram[address as usize], address);
		if address < 0x2000 {
			//Work RAM, 0x0800-0x2000 mirrors 0x0000-0x7FFF
			self.work_ram[address & 0x7FF]
		}
		else if address >= 0x2000 && address < 0x4000 {
			//PPU registers
			self.ppu.read_u8(address)
		}
		else if address >= 0x4000 && address < 0x4020 {
			//I/O Registers
			0
		}
		else if address >= 0x4020 && address < 0x6000 {
			//Expansion ROM
			0
		}
		else if address >= 0x6000 && address < 0x8000 {
			//SRAM
			self.mapper.read_u8(address)
		}
		else if address >= 0x8000 && address <= 0xFFFF {
			//PRG-ROM
			self.mapper.read_u8(address)
		}
		else {
			unreachable!();
		}

    }

    pub fn write_u8(&mut self, address: usize, data: u8) {
        //println!("Writing {:x?} to {:x?}", data, address);
        if address < 0x2000 {
			//Work RAM, 0x0800-0x2000 mirrors 0x0000-0x7FFF
			self.work_ram[address & 0x7FF] = data;
		}
		else if address >= 0x2000 && address < 0x4000 {
			//I/O Registers
			//0x2008 - 0x4000 mirror 0x2000-0x2007
			
		}
		else if address >= 0x4000 && address < 0x4020 {
			//I/O Registers
			
		}
		else if address >= 0x4020 && address < 0x6000 {
			//Expansion ROM
			
		}
		else if address >= 0x6000 && address < 0x8000 {
			//SRAM
			self.mapper.write_u8(address, data);
		}
		else if address >= 0x8000 && address <= 0xFFFF {
			//PRG-ROM
			//should we allow writes here??
		}
		else {
			unreachable!();
		}
    }
}