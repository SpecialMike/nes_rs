use std::io::Read;

//todo: will this just be mapper 0, or will this be abstracted away to determine which mapper type this is, and then implement each one in its own file?

enum Type {
    NROM = 0,
    UNROM,
}

enum Flags6 {
    //Hard-wired nametable mirroring type
    //0: Horizontal or mapper-controlled
    //1: Vertical
    M = 0,
    //"Battery" and other non-volatle memory present
    //0: Not present, 1: Present
    B,
    //512-byte trainer
    //0: Not present
    //1: Present
    T,
    //Hard-wired four-screen mode
    //0: No,
    //1: Yes,
    F,
    //4 bits (low nibble) for mapper number
    N,
}

enum Flags7 {
    //2 bits
    //Console type
    //0: NES/Famicom, 1: Nintendo Vs. System, 2: Nintendo Playchoice 10, 3: Extended Console Type
    T = 0,
    //two bits identifying that this is an NES 2.0 filetype
    ID = 2,
    //4 bites (high nibble) for mapper number
    N = 4,
}

enum TimingMode {
    NTSC = 0,
    PAL,
    Multi,
    Dendy,
}

pub struct Mapper {
    mapper_type: Type,
    mapper_number: u16,
    prg_rom_size: usize,
    chr_rom_size: usize,
    sub_mapper_number: u8,
    prg_ram_size: usize,
    nv_ram_size: usize,
    chr_ram_size: usize,
    chr_nv_ram_size: usize,
    timing_mode: TimingMode,
    num_misc_roms: u8,
    default_expansion_device: u8,

    prg_ram: Vec<u8>,
    chr_ram: Vec<u8>,
    prg_rom: Vec<u8>,
    chr_rom: Vec<u8>,
}
impl Mapper {
    pub fn new(filename: &str) -> Mapper {
        let mut file = match std::fs::OpenOptions::new().read(true).open(filename) {
            Ok(f) => f,
            Err(e) => panic!("{}", e),
        };
        let mut buffer: [u8; 16] = [0; 16];
        match file.read(&mut buffer) {
            Ok(_) => {}
            Err(e) => panic!("{}", e),
        };
        println!("{:X?} header", buffer);
        let string = match std::str::from_utf8(&buffer[0..3]) {
            Ok(s) => s,
            Err(e) => panic!("{}", e),
        };
        assert_eq!(string, "NES");
		assert_eq!(buffer[3], 0x1A);
		//rom size in increments of 16 KiB (i.e. 1 is 16KiB, 2 is 32KiB, etc.)
        let prg_rom_size = 0x3FFF *(buffer[4] as u16 | (((buffer[9] & 0x0F) as u16) << 8)) as usize;
        let chr_rom_size = 0x3FFF *(buffer[5] as u16 | (((buffer[9] & 0xF0) as u16) << 4)) as usize;
        let mapper_number = (buffer[6] & 0xF0) as u16 >> 4
            | (buffer[7] & 0xF0) as u16
			| (((buffer[8] & 0x0F) as u16) << 8);
		println!("buffer 10: {:0>8b}", buffer[10]);
		println!("buffer 11: {:0>8b}", buffer[11]);
        let sub_mapper_number = (buffer[8] & 0xF0) >> 4;
        let prg_ram_size_shift = buffer[10] & 0x0F;
        let prg_ram_size = match prg_ram_size_shift == 0 {
            true => 0,
            false => 64 << prg_ram_size_shift,
        };
        let nv_ram_size_shift = (buffer[10] & 0xF0) >> 4;
        let nv_ram_size = match nv_ram_size_shift == 0 {
            true => 0,
            false => 64 << nv_ram_size_shift,
        };

        let chr_ram_size_shift = buffer[11] & 0x0F;
        let chr_ram_size = match chr_ram_size_shift == 0 {
            true => 0,
            false => 64 << chr_ram_size_shift,
        };
        let chr_nv_ram_size_shift = (buffer[11] & 0xF0) >> 4;
        let chr_nv_ram_size = match chr_nv_ram_size_shift == 0 {
            true => 0,
            false => 64 << chr_nv_ram_size_shift,
        };
        let timing_mode = match buffer[12] {
            0 => TimingMode::NTSC,
            1 => TimingMode::PAL,
            2 => TimingMode::Multi,
            3 => TimingMode::Dendy,
            _ => panic!("Encountered timing mode that is not 0-3!"),
        };
        let num_misc_roms = buffer[14];
        let default_expansion_device = buffer[15];

        //todo: non-volatile ram
        let prg_ram = vec![0; prg_ram_size];
		let chr_ram = vec![0; chr_ram_size];
		println!("PRG_RAM is {}", prg_ram_size);
		println!("CHR_RAM is {}", chr_ram_size);
        let mut prg_rom = vec![0; prg_rom_size+1];
		let mut chr_rom = vec![0; chr_rom_size+1];
		println!("PRG_ROM is {:0>4X}", prg_rom_size);
		println!("CHR_ROM is {:0>4X}", chr_rom_size);
		//load the ROM areas
		match file.read(&mut prg_rom) {
			Ok(_) => {}
            Err(e) => panic!("{}", e),
		}
		match file.read(&mut chr_rom) {
			Ok(_) => {}
            Err(e) => panic!("{}", e),
		}

        Mapper {
            mapper_type: Type::NROM,
            mapper_number,
            prg_rom_size,
            chr_rom_size,
            sub_mapper_number,
            prg_ram_size,
            nv_ram_size,
            chr_ram_size,
            chr_nv_ram_size,
            timing_mode,
            num_misc_roms,
            default_expansion_device,
            prg_ram,
            chr_ram,
            prg_rom,
            chr_rom,
        }
    }

    pub fn read_u8(&self, address: usize) -> u8 {
        if address >= 0x6000 && address < 0x8000 {
            //PRG-RAM
            let fixed_address = (address - 0x6000) & self.prg_ram_size;
			self.prg_ram[fixed_address]
        } else if address >= 0x8000 && address <= 0xFFFF {
            //PRG-ROM
            let fixed_address = (address - 0x8000) & self.prg_rom_size;
			let data = self.prg_rom[fixed_address];
			//println!("Read {:0>2X} from {:0>4X}({:0>4X})", data, address, fixed_address);
			data
        } else {
            unreachable!()
        }
    }

    pub fn write_u8(&mut self, address: usize, data: u8) {
        if address >= 0x6000 && address < 0x8000 {
			//PRG-RAM
			let fixed_address = (address - 0x6000) & self.prg_ram_size;
			self.prg_ram[fixed_address] = data;
        } else if address >= 0x8000 && address <= 0xFFFF {
			//PRG-ROM
			let fixed_address = (address - 0x8000) & self.prg_rom_size;
			self.prg_rom[fixed_address] = data;
        }
    }
}
