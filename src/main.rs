mod cpu;
use cpu::CPU;

mod mapper;
use mapper::Mapper;

mod bus;
use bus::Bus;

mod ppu;
use ppu::PPU;

//set up pixels crate for drawing: https://github.com/parasyte/pixels
fn main() {
	let mut cpu = CPU::new("nestest.nes");
	while !cpu.stopped {
		cpu.step();
	}
	println!("0x0002: {}", cpu.bus.read_u8(0x0002));
	println!("0x0003: {}", cpu.bus.read_u8(0x0003));
}

#[cfg(test)]
mod tests {
    use crate::CPU;
    #[test]
    fn cpu() {
		let mut cpu = CPU::new("nestest.nes");
		while !cpu.stopped {
			cpu.step();
		}
		assert_eq!(0x0000, cpu.bus.read_u8(0x0002));
		assert_eq!(0x0000, cpu.bus.read_u8(0x0003));
    }
}

