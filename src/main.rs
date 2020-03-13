mod cpu;
use cpu::CPU;

use std::io::Read;
use std::io::Seek;

//set up pixels crate for drawing: https://github.com/parasyte/pixels
fn main() {
	let mut cpu = CPU::new();
	//load file into RAM at C000
	let mut file  = match std::fs::OpenOptions::new().read(true).open("nestest.nes"){
		Ok(f) => f,
		Err(e) => panic!("{}", e)
	};
	match file.seek(std::io::SeekFrom::Start(0x10)){
		Ok(_) => {},
		Err(e) => panic!(format!("Error seeking in file! {}", e)),
	};
	let prog_base_addr: usize = 0xC000;
	let prog_size: u64 = match file.metadata(){
		Ok(m) => m.len(),
		Err(e) => panic!("{}", e),
	};
	let offset_max = if prog_base_addr+prog_size as usize > 0xFFFF {0xFFFF} else {prog_base_addr+prog_size as usize};
	match file.read(&mut cpu.mmu.ram[prog_base_addr..offset_max]) {
		Ok(_) => {},
		Err(e) => panic!("{}", e),
	};
	while !cpu.stopped {
		cpu.step();
	}
}