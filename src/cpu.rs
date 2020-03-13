#[repr(u8)]
pub enum StatusFlag {
    Carry = 0,
    Zero,
    InterruptDisable,
    Decimal,
    Break,
    _Unused,
    Overflow, //V
    Negative,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum AddressingMode {
    Implicit,
    Immediate,
    ABS,
    ABX,
    ABY,
    ZP,
    ZPX,
    ZPY,
    REL,
    IND,
    IZX,
    IZY,
}

#[derive(Clone)]
struct Instr {
    mode: AddressingMode,
    operation: fn(&mut CPU) -> (),
    addr_low: Option<u8>, //data held for the instruction
    addr_high: Option<u8>,
	pointer_low: Option<u8>, //used for indirect addressing (idx, idy)
	pointer_high: Option<u8>,
    data: Option<u8>,
	fix_high_byte: bool, //used to denote if we need to take an extra cycle to fix addr_high in some addressing modes
	name: String,
	op: u8,//for display purposes only!
	byte1: u8,
	byte2: u8,
}
impl Instr {
    pub fn get_address(&self) -> Option<u16> {
        if self.addr_high.is_some() && self.addr_low.is_some() {
            Some(low_hi_to_u16(
                self.addr_low.unwrap(),
                self.addr_high.unwrap(),
            ))
        } else {
            None
        }
    }
    pub fn get_pointer(&self) -> Option<u16> {
        if self.pointer_low.is_some() {
			let ptr_high = match self.pointer_high {
				Some(p) => p,
				None => 0x00
			};
            Some(low_hi_to_u16(self.pointer_low.unwrap(), ptr_high))
        } else {
            None
        }
    }
    pub fn new(mode: AddressingMode, operation: fn(&mut CPU) -> (), name: String, op: u8, byte1: u8, byte2: u8) -> Instr {
        Instr {
            mode,
            operation,
            addr_low: None,
            addr_high: None,
			pointer_low: None,
			pointer_high: None,
            data: None,
			fix_high_byte: false,
			name,
			op,
			byte1,
			byte2,
        }
	}
	
	fn print_self(&self, cpu: &CPU) {
		//break out the address and stack peeking into separate methods that this can use, in addition to the actual operators... this way we can print out the data correctly.
		match self.mode {
			AddressingMode::Implicit => 	{
				let write_to_a = self.op == 0x4A || self.op == 0x0A || self.op == 0x6A || self.op == 0x2A;
				let target = if write_to_a {'A'} else {' '};
											print!("{:0>2X}         \t{} {}    \t\t", self.op, self.name, target);
			},
    		AddressingMode::Immediate => 	print!("{:0>2X} {:0>2X}   \t{} #${:0>2X}\t\t", self.op, self.byte1, self.name, self.byte1),
    		AddressingMode::ABS =>{
				if self.op == 0x4C || self.op == 0x20 {//jmp or jsr
											print!("{:0>2X} {:0>2X} {:0>2X}\t{} ${:0>4X}\t\t", self.op, self.byte1, self.byte2, self.name, low_hi_to_u16(self.byte1, self.byte2));
				}
				else {
											print!("{:0>2X} {:0>2X} {:0>2X}\t{} ${:0>4X} = {:0>2X}\t", self.op, self.byte1, self.byte2, self.name, low_hi_to_u16(self.byte1, self.byte2), cpu.mmu.get_u8(low_hi_to_u16(self.byte1, self.byte2)));
				}
			}
    		AddressingMode::ABX => 			print!("{:0>2X} {:0>2X} {:0>2X}\t{} ${:0>4X},X\t\t", self.op, self.byte1, self.byte2, self.name, low_hi_to_u16(self.byte1, self.byte2)),
    		AddressingMode::ABY => 			print!("{:0>2X} {:0>2X} {:0>2X}\t{} ${:0>2X}{:0>2X},Y\t\t", self.op, self.byte1, self.byte2, self.name, self.byte2, self.byte1),
    		AddressingMode::ZP => 			print!("{:0>2X} {:0>2X}   \t{} ${:0>2X} = {:0>2X}   \t", self.op, self.byte1, self.name, self.byte1, cpu.mmu.get_u8(low_hi_to_u16(self.byte1, 0x00))),
    		AddressingMode::ZPX => 			print!("{:0>2X} {:0>2X}   \t{} ${:0>2X},X\t\t", self.op, self.byte1, self.name, self.byte1),
    		AddressingMode::ZPY => 			print!("{:0>2X}{}", self.op, self.name),
    		AddressingMode::REL => 			print!("{:0>2X} {:0>2X}   \t{} ${:0>4X}\t\t", self.op, self.byte1, self.name, low_hi_to_u16(self.byte1, 0x00) + cpu.pc + 0x2),
    		AddressingMode::IND => 			{
				let pointer = low_hi_to_u16(self.byte1, self.byte2);
				let pointer_offset = ((pointer + 1) & 0xFF) | (pointer & 0xFF00);
											print!("{:0>2X} {:0>2X} {:0>2X}\t{} (${:0>4X}) = {:0>4X}", self.op, self.byte1, self.byte2, self.name, pointer, low_hi_to_u16(*cpu.mmu.get_u8(pointer),*cpu.mmu.get_u8(pointer_offset)));
			},
    		AddressingMode::IZX => 			print!("{:0>2X} {:0>2X}    \t{} (${:0>2X},X) \t", self.op, self.byte1, self.name, self.byte1),
    		AddressingMode::IZY => 			print!("{:0>2X} {:0>2X}    \t{} (${:0>2X}),Y \t", self.op, self.byte1, self.name, self.byte1),
		}
	}
}

fn low_hi_to_u16(low: u8, high: u8) -> u16 {
    ((high as u16) << 8) | low as u16
}

pub struct CPU {
    pc: u16,
    sp: u8,
    p: u8,
    a: u8,
    x: u8,
    y: u8,
    pub mmu: MMU,
    curr_instruction: Option<Instr>,
    curr_cycle: u8,
	pub stopped: bool,
	cycles: u64,
}
impl CPU {
    pub fn new() -> CPU {
        CPU {
            pc: 0xC000,
            sp: 0xFD,
            p: 0x24,
            a: 0,
            x: 0,
            y: 0,
            mmu: MMU {
                ram: vec![0; 0x10000],
            },
            curr_instruction: None,
            curr_cycle: 2,
			stopped: false,
			cycles: 6,
        }
    }
    pub fn get_status_flag(&self, flag: StatusFlag) -> bool {
        (self.p >> (flag as u8)) & 0x1 == 0x1
    }
    pub fn set_status_flag(&mut self, flag: StatusFlag, value: bool) {
        if value {
            self.p = self.p | (0x1 << (flag as u8));
        } else {
            self.p = self.p & !(0x1 << (flag as u8));
        }
    }
    fn set_n_z_flags(&mut self, data: u8) {
        self.set_status_flag(StatusFlag::Negative, data & 0b1000_0000 == 0b1000_0000);
        self.set_status_flag(StatusFlag::Zero, data == 0);
	}

    fn decode_instr(op: u8, byte1: u8, byte2: u8) -> Instr {
        //this is probably faster than trying to do some decoding logic anyway...
        match op {
            0x0 => Instr::new(AddressingMode::Implicit, CPU::brk, "BRK".to_string(), op, byte1, byte2),
            0x1 => Instr::new(AddressingMode::IZX, CPU::ora, "ORA".to_string(), op, byte1, byte2),
            0x2 => Instr::new(AddressingMode::Implicit, CPU::stp, "STP".to_string(), op, byte1, byte2),
            0x3 => Instr::new(AddressingMode::IZX, CPU::slo, "SLO".to_string(), op, byte1, byte2),
            0x4 => Instr::new(AddressingMode::ZP, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0x5 => Instr::new(AddressingMode::ZP, CPU::ora, "ORA".to_string(), op, byte1, byte2),
            0x6 => Instr::new(AddressingMode::ZP, CPU::asl, "ASL".to_string(), op, byte1, byte2),
            0x7 => Instr::new(AddressingMode::ZP, CPU::slo, "SLO".to_string(), op, byte1, byte2),
            0x8 => Instr::new(AddressingMode::Implicit, CPU::php, "PHP".to_string(), op, byte1, byte2),
            0x9 => Instr::new(AddressingMode::Immediate, CPU::ora, "ORA".to_string(), op, byte1, byte2),
            0xA => Instr::new(AddressingMode::Implicit, CPU::asl, "ASL".to_string(), op, byte1, byte2),
            0xB => Instr::new(AddressingMode::Immediate, CPU::anc, "ANC".to_string(), op, byte1, byte2),
            0xC => Instr::new(AddressingMode::ABS, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0xD => Instr::new(AddressingMode::ABS, CPU::ora, "ORA".to_string(), op, byte1, byte2),
            0xE => Instr::new(AddressingMode::ABS, CPU::asl, "ASL".to_string(), op, byte1, byte2),
            0xF => Instr::new(AddressingMode::ABS, CPU::slo, "SLO".to_string(), op, byte1, byte2),
            0x10 => Instr::new(AddressingMode::REL, CPU::bpl, "BPL".to_string(), op, byte1, byte2),
            0x11 => Instr::new(AddressingMode::IZY, CPU::ora, "ORA".to_string(), op, byte1, byte2),
            0x12 => Instr::new(AddressingMode::Implicit, CPU::stp, "STP".to_string(), op, byte1, byte2),
            0x13 => Instr::new(AddressingMode::IZY, CPU::slo, "SLO".to_string(), op, byte1, byte2),
            0x14 => Instr::new(AddressingMode::ZPX, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0x15 => Instr::new(AddressingMode::ZPX, CPU::ora, "ORA".to_string(), op, byte1, byte2),
            0x16 => Instr::new(AddressingMode::ZPX, CPU::asl, "ASL".to_string(), op, byte1, byte2),
            0x17 => Instr::new(AddressingMode::ZPX, CPU::slo, "SLO".to_string(), op, byte1, byte2),
            0x18 => Instr::new(AddressingMode::Implicit, CPU::clc, "CLC".to_string(), op, byte1, byte2),
            0x19 => Instr::new(AddressingMode::ABY, CPU::ora, "ORA".to_string(), op, byte1, byte2),
            0x1A => Instr::new(AddressingMode::Implicit, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0x1B => Instr::new(AddressingMode::ABY, CPU::slo, "SLO".to_string(), op, byte1, byte2),
            0x1C => Instr::new(AddressingMode::ABX, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0x1D => Instr::new(AddressingMode::ABX, CPU::ora, "ORA".to_string(), op, byte1, byte2),
            0x1E => Instr::new(AddressingMode::ABX, CPU::asl, "ASL".to_string(), op, byte1, byte2),
            0x1F => Instr::new(AddressingMode::ABX, CPU::slo, "SLO".to_string(), op, byte1, byte2),
            0x20 => Instr::new(AddressingMode::ABS, CPU::jsr, "JSR".to_string(), op, byte1, byte2),
            0x21 => Instr::new(AddressingMode::IZX, CPU::and, "AND".to_string(), op, byte1, byte2),
            0x22 => Instr::new(AddressingMode::Implicit, CPU::stp, "STP".to_string(), op, byte1, byte2),
            0x23 => Instr::new(AddressingMode::IZX, CPU::rla, "RLA".to_string(), op, byte1, byte2),
            0x24 => Instr::new(AddressingMode::ZP, CPU::bit, "BIT".to_string(), op, byte1, byte2),
            0x25 => Instr::new(AddressingMode::ZP, CPU::and, "AND".to_string(), op, byte1, byte2),
            0x26 => Instr::new(AddressingMode::ZP, CPU::rol, "ROL".to_string(), op, byte1, byte2),
            0x27 => Instr::new(AddressingMode::ZP, CPU::rla, "RLA".to_string(), op, byte1, byte2),
            0x28 => Instr::new(AddressingMode::Implicit, CPU::plp, "PLP".to_string(), op, byte1, byte2),
            0x29 => Instr::new(AddressingMode::Immediate, CPU::and, "AND".to_string(), op, byte1, byte2),
            0x2A => Instr::new(AddressingMode::Implicit, CPU::rol, "ROL".to_string(), op, byte1, byte2),
            0x2B => Instr::new(AddressingMode::Immediate, CPU::anc, "ANC".to_string(), op, byte1, byte2),
            0x2C => Instr::new(AddressingMode::ABS, CPU::bit, "BIT".to_string(), op, byte1, byte2),
            0x2D => Instr::new(AddressingMode::ABS, CPU::and, "AND".to_string(), op, byte1, byte2),
            0x2E => Instr::new(AddressingMode::ABS, CPU::rol, "ROL".to_string(), op, byte1, byte2),
            0x2F => Instr::new(AddressingMode::ABS, CPU::rla, "RLA".to_string(), op, byte1, byte2),
            0x30 => Instr::new(AddressingMode::REL, CPU::bmi, "BMI".to_string(), op, byte1, byte2),
            0x31 => Instr::new(AddressingMode::IZY, CPU::and, "AND".to_string(), op, byte1, byte2),
            0x32 => Instr::new(AddressingMode::Implicit, CPU::stp, "STP".to_string(), op, byte1, byte2),
            0x33 => Instr::new(AddressingMode::IZY, CPU::rla, "RLA".to_string(), op, byte1, byte2),
            0x34 => Instr::new(AddressingMode::ZPX, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0x35 => Instr::new(AddressingMode::ZPX, CPU::and, "AND".to_string(), op, byte1, byte2),
            0x36 => Instr::new(AddressingMode::ZPX, CPU::rol, "ROL".to_string(), op, byte1, byte2),
            0x37 => Instr::new(AddressingMode::ZPX, CPU::rla, "RLA".to_string(), op, byte1, byte2),
            0x38 => Instr::new(AddressingMode::Implicit, CPU::sec, "SEC".to_string(), op, byte1, byte2),
            0x39 => Instr::new(AddressingMode::ABY, CPU::and, "AND".to_string(), op, byte1, byte2),
            0x3A => Instr::new(AddressingMode::Implicit, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0x3B => Instr::new(AddressingMode::ABY, CPU::rla, "RLA".to_string(), op, byte1, byte2),
            0x3C => Instr::new(AddressingMode::ABX, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0x3D => Instr::new(AddressingMode::ABX, CPU::and, "AND".to_string(), op, byte1, byte2),
            0x3E => Instr::new(AddressingMode::ABX, CPU::rol, "ROL".to_string(), op, byte1, byte2),
            0x3F => Instr::new(AddressingMode::ABX, CPU::rla, "RLA".to_string(), op, byte1, byte2),
            0x40 => Instr::new(AddressingMode::Implicit, CPU::rti, "RTI".to_string(), op, byte1, byte2),
            0x41 => Instr::new(AddressingMode::IZX, CPU::eor, "EOR".to_string(), op, byte1, byte2),
            0x42 => Instr::new(AddressingMode::Implicit, CPU::stp, "STP".to_string(), op, byte1, byte2),
            0x43 => Instr::new(AddressingMode::IZX, CPU::sre, "SRE".to_string(), op, byte1, byte2),
            0x44 => Instr::new(AddressingMode::ZP, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0x45 => Instr::new(AddressingMode::ZP, CPU::eor, "EOR".to_string(), op, byte1, byte2),
            0x46 => Instr::new(AddressingMode::ZP, CPU::lsr, "LSR".to_string(), op, byte1, byte2),
            0x47 => Instr::new(AddressingMode::ZP, CPU::sre, "SRE".to_string(), op, byte1, byte2),
            0x48 => Instr::new(AddressingMode::Implicit, CPU::pha, "PHA".to_string(), op, byte1, byte2),
            0x49 => Instr::new(AddressingMode::Immediate, CPU::eor, "EOR".to_string(), op, byte1, byte2),
            0x4A => Instr::new(AddressingMode::Implicit, CPU::lsr, "LSR".to_string(), op, byte1, byte2),
            0x4B => Instr::new(AddressingMode::Immediate, CPU::alr, "ALR".to_string(), op, byte1, byte2),
            0x4C => Instr::new(AddressingMode::ABS, CPU::jmp, "JMP".to_string(), op, byte1, byte2),
            0x4D => Instr::new(AddressingMode::ABS, CPU::eor, "EOR".to_string(), op, byte1, byte2),
            0x4E => Instr::new(AddressingMode::ABS, CPU::lsr, "LSR".to_string(), op, byte1, byte2),
            0x4F => Instr::new(AddressingMode::ABS, CPU::sre, "SRE".to_string(), op, byte1, byte2),
            0x50 => Instr::new(AddressingMode::REL, CPU::bvc, "BVC".to_string(), op, byte1, byte2),
            0x51 => Instr::new(AddressingMode::IZY, CPU::eor, "EOR".to_string(), op, byte1, byte2),
            0x52 => Instr::new(AddressingMode::Implicit, CPU::stp, "STP".to_string(), op, byte1, byte2),
            0x53 => Instr::new(AddressingMode::IZY, CPU::sre, "SRE".to_string(), op, byte1, byte2),
            0x54 => Instr::new(AddressingMode::ZPX, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0x55 => Instr::new(AddressingMode::ZPX, CPU::eor, "EOR".to_string(), op, byte1, byte2),
            0x56 => Instr::new(AddressingMode::ZPX, CPU::lsr, "LSR".to_string(), op, byte1, byte2),
            0x57 => Instr::new(AddressingMode::ZPX, CPU::sre, "SRE".to_string(), op, byte1, byte2),
            0x58 => Instr::new(AddressingMode::Implicit, CPU::cli, "CLI".to_string(), op, byte1, byte2),
            0x59 => Instr::new(AddressingMode::ABY, CPU::eor, "EOR".to_string(), op, byte1, byte2),
            0x5A => Instr::new(AddressingMode::Implicit, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0x5B => Instr::new(AddressingMode::ABY, CPU::sre, "SRE".to_string(), op, byte1, byte2),
            0x5C => Instr::new(AddressingMode::ABX, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0x5D => Instr::new(AddressingMode::ABX, CPU::eor, "EOR".to_string(), op, byte1, byte2),
            0x5E => Instr::new(AddressingMode::ABX, CPU::lsr, "LSR".to_string(), op, byte1, byte2),
            0x5F => Instr::new(AddressingMode::ABX, CPU::sre, "SRE".to_string(), op, byte1, byte2),
            0x60 => Instr::new(AddressingMode::Implicit, CPU::rts, "RTS".to_string(), op, byte1, byte2),
            0x61 => Instr::new(AddressingMode::IZX, CPU::adc, "ADC".to_string(), op, byte1, byte2),
            0x62 => Instr::new(AddressingMode::Implicit, CPU::stp, "STP".to_string(), op, byte1, byte2),
            0x63 => Instr::new(AddressingMode::IZX, CPU::rra, "RRA".to_string(), op, byte1, byte2),
            0x64 => Instr::new(AddressingMode::ZP, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0x65 => Instr::new(AddressingMode::ZP, CPU::adc, "ADC".to_string(), op, byte1, byte2),
            0x66 => Instr::new(AddressingMode::ZP, CPU::ror, "ROR".to_string(), op, byte1, byte2),
            0x67 => Instr::new(AddressingMode::ZP, CPU::rra, "RRA".to_string(), op, byte1, byte2),
            0x68 => Instr::new(AddressingMode::Implicit, CPU::pla, "PLA".to_string(), op, byte1, byte2),
            0x69 => Instr::new(AddressingMode::Immediate, CPU::adc, "ADC".to_string(), op, byte1, byte2),
            0x6A => Instr::new(AddressingMode::Implicit, CPU::ror, "ROR".to_string(), op, byte1, byte2),
            0x6B => Instr::new(AddressingMode::Immediate, CPU::arr, "ARR".to_string(), op, byte1, byte2),
            0x6C => Instr::new(AddressingMode::IND, CPU::jmp, "JMP".to_string(), op, byte1, byte2),
            0x6D => Instr::new(AddressingMode::ABS, CPU::adc, "ADC".to_string(), op, byte1, byte2),
            0x6E => Instr::new(AddressingMode::ABS, CPU::ror, "ROR".to_string(), op, byte1, byte2),
            0x6F => Instr::new(AddressingMode::ABS, CPU::rra, "RRA".to_string(), op, byte1, byte2),
            0x70 => Instr::new(AddressingMode::REL, CPU::bvs, "BVS".to_string(), op, byte1, byte2),
            0x71 => Instr::new(AddressingMode::IZY, CPU::adc, "ADC".to_string(), op, byte1, byte2),
            0x72 => Instr::new(AddressingMode::Implicit, CPU::stp, "STP".to_string(), op, byte1, byte2),
            0x73 => Instr::new(AddressingMode::IZY, CPU::rra, "RRA".to_string(), op, byte1, byte2),
            0x74 => Instr::new(AddressingMode::ZPX, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0x75 => Instr::new(AddressingMode::ZPX, CPU::adc, "ADC".to_string(), op, byte1, byte2),
            0x76 => Instr::new(AddressingMode::ZPX, CPU::ror, "ROR".to_string(), op, byte1, byte2),
            0x77 => Instr::new(AddressingMode::ZPX, CPU::rra, "RRA".to_string(), op, byte1, byte2),
            0x78 => Instr::new(AddressingMode::Implicit, CPU::sei, "SEI".to_string(), op, byte1, byte2),
            0x79 => Instr::new(AddressingMode::ABY, CPU::adc, "ADC".to_string(), op, byte1, byte2),
            0x7A => Instr::new(AddressingMode::Implicit, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0x7B => Instr::new(AddressingMode::ABY, CPU::rra, "RRA".to_string(), op, byte1, byte2),
            0x7C => Instr::new(AddressingMode::ABX, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0x7D => Instr::new(AddressingMode::ABX, CPU::adc, "ADC".to_string(), op, byte1, byte2),
            0x7E => Instr::new(AddressingMode::ABX, CPU::ror, "ROR".to_string(), op, byte1, byte2),
            0x7F => Instr::new(AddressingMode::ABX, CPU::rra, "RRA".to_string(), op, byte1, byte2),
            0x80 => Instr::new(AddressingMode::Immediate, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0x81 => Instr::new(AddressingMode::IZX, CPU::sta, "STA".to_string(), op, byte1, byte2),
            0x82 => Instr::new(AddressingMode::Immediate, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0x83 => Instr::new(AddressingMode::IZX, CPU::sax, "SAX".to_string(), op, byte1, byte2),
            0x84 => Instr::new(AddressingMode::ZP, CPU::sty, "STY".to_string(), op, byte1, byte2),
            0x85 => Instr::new(AddressingMode::ZP, CPU::sta, "STA".to_string(), op, byte1, byte2),
            0x86 => Instr::new(AddressingMode::ZP, CPU::stx, "STX".to_string(), op, byte1, byte2),
            0x87 => Instr::new(AddressingMode::ZP, CPU::sax, "SAX".to_string(), op, byte1, byte2),
            0x88 => Instr::new(AddressingMode::Implicit, CPU::dey, "DEY".to_string(), op, byte1, byte2),
            0x89 => Instr::new(AddressingMode::Immediate, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0x8A => Instr::new(AddressingMode::Implicit, CPU::txa, "TXA".to_string(), op, byte1, byte2),
            0x8B => Instr::new(AddressingMode::Immediate, CPU::xaa, "XAA".to_string(), op, byte1, byte2),
            0x8C => Instr::new(AddressingMode::ABS, CPU::sty, "STY".to_string(), op, byte1, byte2),
            0x8D => Instr::new(AddressingMode::ABS, CPU::sta, "STA".to_string(), op, byte1, byte2),
            0x8E => Instr::new(AddressingMode::ABS, CPU::stx, "STX".to_string(), op, byte1, byte2),
            0x8F => Instr::new(AddressingMode::ABS, CPU::sax, "SAX".to_string(), op, byte1, byte2),
            0x90 => Instr::new(AddressingMode::REL, CPU::bcc, "BCC".to_string(), op, byte1, byte2),
            0x91 => Instr::new(AddressingMode::IZY, CPU::sta, "STA".to_string(), op, byte1, byte2),
            0x92 => Instr::new(AddressingMode::Implicit, CPU::stp, "STP".to_string(), op, byte1, byte2),
            0x93 => Instr::new(AddressingMode::IZY, CPU::ahx, "AHX".to_string(), op, byte1, byte2),
            0x94 => Instr::new(AddressingMode::ZPX, CPU::sty, "STY".to_string(), op, byte1, byte2),
            0x95 => Instr::new(AddressingMode::ZPX, CPU::sta, "STA".to_string(), op, byte1, byte2),
            0x96 => Instr::new(AddressingMode::ZPY, CPU::stx, "STX".to_string(), op, byte1, byte2),
            0x97 => Instr::new(AddressingMode::ZPY, CPU::sax, "SAX".to_string(), op, byte1, byte2),
            0x98 => Instr::new(AddressingMode::Implicit, CPU::tya, "TYA".to_string(), op, byte1, byte2),
            0x99 => Instr::new(AddressingMode::ABY, CPU::sta, "STA".to_string(), op, byte1, byte2),
            0x9A => Instr::new(AddressingMode::Implicit, CPU::txs, "TXS".to_string(), op, byte1, byte2),
            0x9B => Instr::new(AddressingMode::ABY, CPU::tas, "TAS".to_string(), op, byte1, byte2),
            0x9C => Instr::new(AddressingMode::ABX, CPU::shy, "SHY".to_string(), op, byte1, byte2),
            0x9D => Instr::new(AddressingMode::ABX, CPU::sta, "STA".to_string(), op, byte1, byte2),
            0x9E => Instr::new(AddressingMode::ABY, CPU::shx, "SHX".to_string(), op, byte1, byte2),
            0x9F => Instr::new(AddressingMode::ABY, CPU::ahx, "AHX".to_string(), op, byte1, byte2),
            0xA0 => Instr::new(AddressingMode::Immediate, CPU::ldy, "LDY".to_string(), op, byte1, byte2),
            0xA1 => Instr::new(AddressingMode::IZX, CPU::lda, "LDA".to_string(), op, byte1, byte2),
            0xA2 => Instr::new(AddressingMode::Immediate, CPU::ldx, "LDX".to_string(), op, byte1, byte2),
            0xA3 => Instr::new(AddressingMode::IZX, CPU::lax, "LAX".to_string(), op, byte1, byte2),
            0xA4 => Instr::new(AddressingMode::ZP, CPU::ldy, "LDY".to_string(), op, byte1, byte2),
            0xA5 => Instr::new(AddressingMode::ZP, CPU::lda, "LDA".to_string(), op, byte1, byte2),
            0xA6 => Instr::new(AddressingMode::ZP, CPU::ldx, "LDX".to_string(), op, byte1, byte2),
            0xA7 => Instr::new(AddressingMode::ZP, CPU::lax, "LAX".to_string(), op, byte1, byte2),
            0xA8 => Instr::new(AddressingMode::Implicit, CPU::tay, "TAY".to_string(), op, byte1, byte2),
            0xA9 => Instr::new(AddressingMode::Immediate, CPU::lda, "LDA".to_string(), op, byte1, byte2),
            0xAA => Instr::new(AddressingMode::Implicit, CPU::tax, "TAX".to_string(), op, byte1, byte2),
            0xAB => Instr::new(AddressingMode::Immediate, CPU::lax, "LAX".to_string(), op, byte1, byte2),
            0xAC => Instr::new(AddressingMode::ABS, CPU::ldy, "LDY".to_string(), op, byte1, byte2),
            0xAD => Instr::new(AddressingMode::ABS, CPU::lda, "LDA".to_string(), op, byte1, byte2),
            0xAE => Instr::new(AddressingMode::ABS, CPU::ldx, "LDX".to_string(), op, byte1, byte2),
            0xAF => Instr::new(AddressingMode::ABS, CPU::lax, "LAX".to_string(), op, byte1, byte2),
            0xB0 => Instr::new(AddressingMode::REL, CPU::bcs, "BCS".to_string(), op, byte1, byte2),
            0xB1 => Instr::new(AddressingMode::IZY, CPU::lda, "LDA".to_string(), op, byte1, byte2),
            0xB2 => Instr::new(AddressingMode::Implicit, CPU::stp, "STP".to_string(), op, byte1, byte2),
            0xB3 => Instr::new(AddressingMode::IZY, CPU::lax, "LAX".to_string(), op, byte1, byte2),
            0xB4 => Instr::new(AddressingMode::ZPX, CPU::ldy, "LDY".to_string(), op, byte1, byte2),
            0xB5 => Instr::new(AddressingMode::ZPX, CPU::lda, "LDA".to_string(), op, byte1, byte2),
            0xB6 => Instr::new(AddressingMode::ZPY, CPU::ldx, "LDX".to_string(), op, byte1, byte2),
            0xB7 => Instr::new(AddressingMode::ZPY, CPU::lax, "LAX".to_string(), op, byte1, byte2),
            0xB8 => Instr::new(AddressingMode::Implicit, CPU::clv, "CLV".to_string(), op, byte1, byte2),
            0xB9 => Instr::new(AddressingMode::ABY, CPU::lda, "LDA".to_string(), op, byte1, byte2),
            0xBA => Instr::new(AddressingMode::Implicit, CPU::tsx, "TSX".to_string(), op, byte1, byte2),
            0xBB => Instr::new(AddressingMode::ABY, CPU::las, "LAS".to_string(), op, byte1, byte2),
            0xBC => Instr::new(AddressingMode::ABX, CPU::ldy, "LDY".to_string(), op, byte1, byte2),
            0xBD => Instr::new(AddressingMode::ABX, CPU::lda, "LDA".to_string(), op, byte1, byte2),
            0xBE => Instr::new(AddressingMode::ABY, CPU::ldx, "LDX".to_string(), op, byte1, byte2),
            0xBF => Instr::new(AddressingMode::ABY, CPU::lax, "LAX".to_string(), op, byte1, byte2),
            0xC0 => Instr::new(AddressingMode::Immediate, CPU::cpy, "CPY".to_string(), op, byte1, byte2),
            0xC1 => Instr::new(AddressingMode::IZX, CPU::cmp, "CMP".to_string(), op, byte1, byte2),
            0xC2 => Instr::new(AddressingMode::Immediate, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0xC3 => Instr::new(AddressingMode::IZX, CPU::dcp, "DCP".to_string(), op, byte1, byte2),
            0xC4 => Instr::new(AddressingMode::ZP, CPU::cpy, "CPY".to_string(), op, byte1, byte2),
            0xC5 => Instr::new(AddressingMode::ZP, CPU::cmp, "CMP".to_string(), op, byte1, byte2),
            0xC6 => Instr::new(AddressingMode::ZP, CPU::dec, "DEC".to_string(), op, byte1, byte2),
            0xC7 => Instr::new(AddressingMode::ZP, CPU::dcp, "DCP".to_string(), op, byte1, byte2),
            0xC8 => Instr::new(AddressingMode::Implicit, CPU::iny, "INY".to_string(), op, byte1, byte2),
            0xC9 => Instr::new(AddressingMode::Immediate, CPU::cmp, "CMP".to_string(), op, byte1, byte2),
            0xCA => Instr::new(AddressingMode::Implicit, CPU::dex, "DEX".to_string(), op, byte1, byte2),
            0xCB => Instr::new(AddressingMode::Immediate, CPU::axs, "AXS".to_string(), op, byte1, byte2),
            0xCC => Instr::new(AddressingMode::ABS, CPU::cpy, "CPY".to_string(), op, byte1, byte2),
            0xCD => Instr::new(AddressingMode::ABS, CPU::cmp, "CMP".to_string(), op, byte1, byte2),
            0xCE => Instr::new(AddressingMode::ABS, CPU::dec, "DEC".to_string(), op, byte1, byte2),
            0xCF => Instr::new(AddressingMode::ABS, CPU::dcp, "DCP".to_string(), op, byte1, byte2),
            0xD0 => Instr::new(AddressingMode::REL, CPU::bne, "BNE".to_string(), op, byte1, byte2),
            0xD1 => Instr::new(AddressingMode::IZY, CPU::cmp, "CMP".to_string(), op, byte1, byte2),
            0xD2 => Instr::new(AddressingMode::Implicit, CPU::stp, "STP".to_string(), op, byte1, byte2),
            0xD3 => Instr::new(AddressingMode::IZY, CPU::dcp, "DCP".to_string(), op, byte1, byte2),
            0xD4 => Instr::new(AddressingMode::ZPX, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0xD5 => Instr::new(AddressingMode::ZPX, CPU::cmp, "CMP".to_string(), op, byte1, byte2),
            0xD6 => Instr::new(AddressingMode::ZPX, CPU::dec, "DEC".to_string(), op, byte1, byte2),
            0xD7 => Instr::new(AddressingMode::ZPX, CPU::dcp, "DCP".to_string(), op, byte1, byte2),
            0xD8 => Instr::new(AddressingMode::Implicit, CPU::cld, "CLD".to_string(), op, byte1, byte2),
            0xD9 => Instr::new(AddressingMode::ABY, CPU::cmp, "CMP".to_string(), op, byte1, byte2),
            0xDA => Instr::new(AddressingMode::Implicit, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0xDB => Instr::new(AddressingMode::ABY, CPU::dcp, "DCP".to_string(), op, byte1, byte2),
            0xDC => Instr::new(AddressingMode::ABX, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0xDD => Instr::new(AddressingMode::ABX, CPU::cmp, "CMP".to_string(), op, byte1, byte2),
            0xDE => Instr::new(AddressingMode::ABX, CPU::dec, "DEC".to_string(), op, byte1, byte2),
            0xDF => Instr::new(AddressingMode::ABX, CPU::dcp, "DCP".to_string(), op, byte1, byte2),
            0xE0 => Instr::new(AddressingMode::Immediate, CPU::cpx, "CPX".to_string(), op, byte1, byte2),
            0xE1 => Instr::new(AddressingMode::IZX, CPU::sbc, "SBC".to_string(), op, byte1, byte2),
            0xE2 => Instr::new(AddressingMode::Immediate, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0xE3 => Instr::new(AddressingMode::IZX, CPU::isb, "ISB".to_string(), op, byte1, byte2),
            0xE4 => Instr::new(AddressingMode::ZP, CPU::cpx, "CPX".to_string(), op, byte1, byte2),
            0xE5 => Instr::new(AddressingMode::ZP, CPU::sbc, "SBC".to_string(), op, byte1, byte2),
            0xE6 => Instr::new(AddressingMode::ZP, CPU::inc, "INC".to_string(), op, byte1, byte2),
            0xE7 => Instr::new(AddressingMode::ZP, CPU::isb, "ISB".to_string(), op, byte1, byte2),
            0xE8 => Instr::new(AddressingMode::Implicit, CPU::inx, "INX".to_string(), op, byte1, byte2),
            0xE9 => Instr::new(AddressingMode::Immediate, CPU::sbc, "SBC".to_string(), op, byte1, byte2),
            0xEA => Instr::new(AddressingMode::Implicit, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0xEB => Instr::new(AddressingMode::Immediate, CPU::sbc, "SBC".to_string(), op, byte1, byte2),
            0xEC => Instr::new(AddressingMode::ABS, CPU::cpx, "CPX".to_string(), op, byte1, byte2),
            0xED => Instr::new(AddressingMode::ABS, CPU::sbc, "SBC".to_string(), op, byte1, byte2),
            0xEE => Instr::new(AddressingMode::ABS, CPU::inc, "INC".to_string(), op, byte1, byte2),
            0xEF => Instr::new(AddressingMode::ABS, CPU::isb, "ISB".to_string(), op, byte1, byte2),
            0xF0 => Instr::new(AddressingMode::REL, CPU::beq, "BEQ".to_string(), op, byte1, byte2),
            0xF1 => Instr::new(AddressingMode::IZY, CPU::sbc, "SBC".to_string(), op, byte1, byte2),
            0xF2 => Instr::new(AddressingMode::Implicit, CPU::stp, "STP".to_string(), op, byte1, byte2),
            0xF3 => Instr::new(AddressingMode::IZY, CPU::isb, "ISB".to_string(), op, byte1, byte2),
            0xF4 => Instr::new(AddressingMode::ZPX, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0xF5 => Instr::new(AddressingMode::ZPX, CPU::sbc, "SBC".to_string(), op, byte1, byte2),
            0xF6 => Instr::new(AddressingMode::ZPX, CPU::inc, "INC".to_string(), op, byte1, byte2),
            0xF7 => Instr::new(AddressingMode::ZPX, CPU::isb, "ISB".to_string(), op, byte1, byte2),
            0xF8 => Instr::new(AddressingMode::Implicit, CPU::sed, "SED".to_string(), op, byte1, byte2),
            0xF9 => Instr::new(AddressingMode::ABY, CPU::sbc, "SBC".to_string(), op, byte1, byte2),
            0xFA => Instr::new(AddressingMode::Implicit, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0xFB => Instr::new(AddressingMode::ABY, CPU::isb, "ISB".to_string(), op, byte1, byte2),
            0xFC => Instr::new(AddressingMode::ABX, CPU::nop, "NOP".to_string(), op, byte1, byte2),
            0xFD => Instr::new(AddressingMode::ABX, CPU::sbc, "SBC".to_string(), op, byte1, byte2),
            0xFE => Instr::new(AddressingMode::ABX, CPU::inc, "INC".to_string(), op, byte1, byte2),
            0xFF => Instr::new(AddressingMode::ABX, CPU::isb, "ISB".to_string(), op, byte1, byte2),
        }
    }

    pub fn step(&mut self) {
		self.cycles += 1;
        if self.curr_instruction.is_none() {
			let op = self.mmu.get_u8(self.pc);
			let byte1 = *self.mmu.get_u8(self.pc+1);
			let byte2 = *self.mmu.get_u8(self.pc+2);
			let instruction = CPU::decode_instr(*op, byte1, byte2);
			print!("{:0>4X}\t", self.pc);
			instruction.print_self(self);
			print!("\tA:{:0>2X} X:{:0>2X} Y:{:0>2X} P:{:0>2X} SP:{:0>2X} PPU:{:X?},{:X?} CYC:{}\n", self.a, self.x, self.y, self.p, self.sp, 0, 0, self.cycles);
			self.pc += 1;
            self.curr_instruction = Some(instruction);
			self.curr_cycle = 2; //always start with 2 as the cycle, the first cycle is used to get the op from RAM[pc]
            return;
        }
        let instruction = self.get_instr();
		(instruction.operation)(self);
		self.curr_cycle += 1;
    }

    pub fn push_stack(&mut self, data: u8) {
        self.mmu.write_u8(self.sp as u16 + 0x0100, data);
        self.sp -= 1;
    }

    pub fn pop_stack(&mut self) -> u8 {
        //todo: figure out if the stack can wrap... is it ok to just set it to 0xFF on start? if we pop at 0xFF, should it wrap to 0x00 to allow for wrapping, or stack overflow type things, or just cap it at 0xFF?
        if self.sp != 0xFF {
            self.sp += 1;
        }
        let data = *self.mmu.get_u8(self.sp as u16 + 0x0100);
        data
    }

    fn get_instr_mut(&mut self) -> &mut Instr {
        self.curr_instruction.as_mut().unwrap()
    }
    fn get_instr(&self) -> Instr {
        self.curr_instruction.clone().unwrap()
    }

    //todo: a lot of the instructions below could be broken out into micro-ops to reduce code repetition
    //i.e. read_next_instruction_and_throw_away, push_pcl, push_pch, pop_pcl, pop_pch, get_pch(addr), get_pcl(addr)

    fn push_pcl(&mut self) {
        self.push_stack((self.pc & 0x00FF) as u8);
    }
    fn push_pch(&mut self) {
        self.push_stack(((self.pc >> 8) & 0x00FF) as u8);
    }
    fn pop_pcl(&mut self) {
        self.pc = self.pc & 0xFF00 | self.pop_stack() as u16;
    }
    fn pop_pch(&mut self) {
        self.pc = self.pc & 0x00FF | ((self.pop_stack() as u16) << 8);
    }
    fn get_pcl(&mut self, address: u16) {
        self.pc = (self.pc & 0xFF00) | *self.mmu.get_u8(address) as u16;
    }
    fn get_pch(&mut self, address: u16) {
        self.pc = (self.pc & 0x00FF) | ((*self.mmu.get_u8(address) as u16) << 8);
    }
    fn read_instr(&mut self) -> u8 {
        self.pc += 1;
        *self.mmu.get_u8(self.pc - 1)
    }

    //Addressing modes
    fn absolute_read(&mut self) {
        match self.curr_cycle {
            2 => self.get_instr_mut().addr_low = Some(self.read_instr()),
            3 => self.get_instr_mut().addr_high = Some(self.read_instr()),
            4 => {
                self.get_instr_mut().data =
                    Some(*self.mmu.get_u8(self.get_instr().get_address().unwrap()));
            }
            _ => {}
        }
    }
    fn immediate_read(&mut self) {
		match self.curr_cycle {
			2 => {
				self.get_instr_mut().data = Some(self.read_instr());
			}
			_ => {}
		}
	}
    fn zp_read(&mut self) {
        match self.curr_cycle {
            2 => {
                self.get_instr_mut().addr_low = Some(self.read_instr());
                self.get_instr_mut().addr_high = Some(0x00);
            }
            3 => {
                self.get_instr_mut().data =
                    Some(*self.mmu.get_u8(self.get_instr().get_address().unwrap()));
            }
            _ => {}
        }
    }
    fn rel_read(&mut self) {
        //this is for branches only
        match self.curr_cycle {
            2 => {
                self.get_instr_mut().data = Some(self.read_instr());
            }
            _ => {}
        }
    }
    fn ind_read(&mut self) {
		match self.curr_cycle {
			2 => self.get_instr_mut().pointer_low = Some(self.read_instr()),
			3 => self.get_instr_mut().pointer_high = Some(self.read_instr()),
			4 => {
				let pointer = self.get_instr().get_pointer().unwrap();
				self.get_instr_mut().addr_low = Some(*self.mmu.get_u8(pointer));
			}
			5 => {
				let pointer = self.get_instr().get_pointer().unwrap();
				//the pointer can't cross a page boundary!
				let pointer_offset = ((pointer + 1) & 0xFF) | (pointer & 0xFF00);
				self.get_instr_mut().addr_high = Some(*self.mmu.get_u8(pointer_offset));
			}
			_ => panic!("{}", self.curr_cycle)
		}
	}
    fn zpx_read(&mut self) {
        match self.curr_cycle {
            2 => {
                //read address
                self.get_instr_mut().addr_low = Some(self.read_instr());
                self.get_instr_mut().addr_high = Some(0x00);
            }
            3 => {
                //read from address (in case there are side-effects)
                self.mmu.get_u8(self.get_instr().get_address().unwrap());
                //then add X to address
                self.get_instr_mut().addr_low = Some(
                    (self.get_instr_mut().addr_low.unwrap() as u16 + self.x as u16 & 0xFF) as u8,
                );
            }
            4 => {
                self.get_instr_mut().data =
                    Some(*self.mmu.get_u8(self.get_instr().get_address().unwrap()));
            }
            _ => {}
        }
    }
    fn zpy_read(&mut self) {
        match self.curr_cycle {
            2 => {
                //read address
                self.get_instr_mut().addr_low = Some(self.read_instr());
                self.get_instr_mut().addr_high = Some(0x00);
            }
            3 => {
                //read from address (in case there are side-effects)
                self.mmu.get_u8(self.get_instr().get_address().unwrap());
                //then add X to address
                self.get_instr_mut().addr_low = Some(
                    (self.get_instr_mut().addr_low.unwrap() as u16 + self.y as u16 & 0xFF) as u8,
                );
            }
            4 => {
                self.get_instr_mut().data =
                    Some(*self.mmu.get_u8(self.get_instr().get_address().unwrap()));
            }
            _ => {}
        }
    }
    fn abx_read(&mut self) {
        match self.curr_cycle {
            2 => self.get_instr_mut().addr_low = Some(self.read_instr()),
            3 => {
                let addr_low = self.get_instr_mut().addr_low.unwrap();
                let addr_low_offset = addr_low as u16 + self.x as u16;

                self.get_instr_mut().fix_high_byte = addr_low_offset > 0xFF;

                self.get_instr_mut().addr_high = Some(self.read_instr());
                self.get_instr_mut().addr_low = Some((addr_low_offset & 0xFF) as u8);
            }
            4 => {
                let data = self.mmu.get_u8(self.get_instr().get_address().unwrap());
                if self.get_instr().fix_high_byte {
                    self.get_instr_mut().addr_high =
                        Some(self.get_instr_mut().addr_high.unwrap() + 0x1);
                } else {
                    self.get_instr_mut().data = Some(*data);
                }
            }
            5 => {
                self.get_instr_mut().data =
                    Some(*self.mmu.get_u8(self.get_instr().get_address().unwrap()));
            }
            _ => {}
        }
    }
    fn aby_read(&mut self) {
        match self.curr_cycle {
            2 => self.get_instr_mut().addr_low = Some(self.read_instr()),
            3 => {
                let addr_low = self.get_instr_mut().addr_low.unwrap();
                let addr_low_offset = addr_low as u16 + self.y as u16;

                self.get_instr_mut().fix_high_byte = addr_low_offset > 0xFF;

                self.get_instr_mut().addr_high = Some(self.read_instr());
                self.get_instr_mut().addr_low = Some((addr_low_offset & 0xFF) as u8);
            }
            4 => {
                let data = self.mmu.get_u8(self.get_instr().get_address().unwrap());
                if self.get_instr().fix_high_byte {
                    self.get_instr_mut().addr_high =
                        Some(((self.get_instr_mut().addr_high.unwrap() as u16 + 0x1) & 0xFF) as u8);
                } else {
                    self.get_instr_mut().data = Some(*data);
                }
            }
            5 => {
                self.get_instr_mut().data =
                    Some(*self.mmu.get_u8(self.get_instr().get_address().unwrap()));
            }
            _ => {}
        }
    }
    fn izx_read(&mut self) {
        //indexed indirect
        match self.curr_cycle {
            2 => {
                //read pointer
                self.get_instr_mut().pointer_low = Some(self.read_instr());
            }
            3 => {
                let pointer = self.get_instr().get_pointer().unwrap();
                //read from pointer (in case there are side-effects)
                self.mmu.get_u8(pointer);
                //then add X to pointer
                self.get_instr_mut().pointer_low = Some(
                    ((self.get_instr_mut().pointer_low.unwrap() as u16 + self.x as u16) & 0xFF)
                        as u8,
                );
            }
            4 => {
                //get low byte of address
                self.get_instr_mut().addr_low =
                    Some(*self.mmu.get_u8(self.get_instr().get_pointer().unwrap()));
            }
            5 => {
                //get high byte of address
                //this is at pointer + 1, but we have to make sure we don't cross the zero page boundary
                let pointer = (self.get_instr().get_pointer().unwrap() + 1) & 0x00FF;
                self.get_instr_mut().addr_high = Some(*self.mmu.get_u8(pointer));
            }
            6 => {
                //get data
                self.get_instr_mut().data =
                    Some(*self.mmu.get_u8(self.get_instr().get_address().unwrap()));
            }
            _ => {}
        }
    }
    fn izy_read(&mut self) {
		//indirect indexed
		match self.curr_cycle {
			2 => self.get_instr_mut().pointer_low = Some(self.read_instr()),
			3 => {
				let pointer = self.get_instr().get_pointer().unwrap();
				self.get_instr_mut().addr_low = Some(*self.mmu.get_u8(pointer));
			}
			4 => {
				let pointer = (self.get_instr().get_pointer().unwrap() + 1) & 0xFF;
				self.get_instr_mut().addr_high = Some(*self.mmu.get_u8(pointer));
				let addr_low = self.get_instr().addr_low.unwrap();
				let offset_addr_low = addr_low as u16 + self.y as u16;
				self.get_instr_mut().fix_high_byte = offset_addr_low > 0xFF;
				self.get_instr_mut().addr_low = Some((offset_addr_low & 0xFF) as u8);
			}
			5 => {
				let data = *self.mmu.get_u8(self.get_instr().get_address().unwrap());
				if self.get_instr().fix_high_byte {
					let new_address_high = self.get_instr().addr_high.unwrap() as u16 + 1;
					self.get_instr_mut().addr_high = Some((new_address_high & 0xFF) as u8);
					//println!("Taking extra cycle!");
				}
				else{
					//println!("Getting data from {:0>4X}", self.get_instr().get_address().unwrap());
					self.get_instr_mut().data = Some(data);
				}
			}
			6 => {
				//println!("Getting data from {:0>4X}", self.get_instr().get_address().unwrap());
				self.get_instr_mut().data =
                    Some(*self.mmu.get_u8(self.get_instr().get_address().unwrap()));
			}
			_ => {}
		}
    }

    fn handle_addressing_mode_read(&mut self) {
		//println!("Addressing Mode: {:?}", self.get_instr().mode);
        match self.get_instr().mode {
            AddressingMode::ABS => self.absolute_read(),
            AddressingMode::Immediate => self.immediate_read(),
            AddressingMode::ZP => self.zp_read(),
            AddressingMode::REL => self.rel_read(),
            AddressingMode::IND => self.ind_read(),
            AddressingMode::ZPX => self.zpx_read(),
            AddressingMode::ZPY => self.zpy_read(),
            AddressingMode::ABX => self.abx_read(),
            AddressingMode::ABY => self.aby_read(),
            AddressingMode::IZX => self.izx_read(),
			AddressingMode::IZY => self.izy_read(),
			AddressingMode::Implicit => self.get_instr_mut().data = Some(self.a),
            _ => {}
        };
    }

    fn abs_write(&mut self, data: u8) -> bool {
        let address = self.get_instr().get_address().unwrap();
        match self.curr_cycle {
			4=> false,
            5 => {
                self.mmu.write_u8(address, data);
                false
            }
            6 => {
                self.mmu.write_u8(address, data);
                true
            }
            _ => unreachable!(),
        }
    }
    fn zp_write(&mut self, data: u8) -> bool {
        let address = self.get_instr().get_address().unwrap();
        match self.curr_cycle {
			3 => false,//we end up here because we finish getting the data, then try to write it immediately
            4 => {
				self.mmu.write_u8(address, data);
                false
            }
            5 => {
				self.mmu.write_u8(address, data);
                true
            }
            _ => panic!("{}", self.curr_cycle),
        }
    }
    fn rel_write(&mut self) -> bool {
        //only for branching instructions, it is handled separately
        panic!("REL write called!");
        //true
    }
    fn ind_write(&mut self) -> bool {
        true
    }
    fn zpx_write(&mut self, data: u8) -> bool {
        let address = self.get_instr().get_address().unwrap();
        match self.curr_cycle {
			4 => false,
            5 => {
                self.mmu.write_u8(address, data);
                false
            }
            6 => {
                self.mmu.write_u8(address, data);
                true
            }
            _ => panic!("{}", self.curr_cycle),
        }
    }
    fn zpy_write(&mut self) -> bool {
        //should never happen!
        panic!("ZPY write called!");
        //true
    }
    fn abx_write(&mut self, data: u8) -> bool {
        let address = self.get_instr().get_address().unwrap();
		match self.curr_cycle {
			4 => false,
			5 => false,
            6 => {
                self.mmu.write_u8(address, data);
                false
            }
            7 => {
                self.mmu.write_u8(address, data);
                true
            }
            _ => panic!("{}", self.curr_cycle),
        }
    }
    fn aby_write(&mut self, data: u8) -> bool {
        let address = self.get_instr().get_address().unwrap();
		match self.curr_cycle {
			4 => false,
			5 => false,
            6 => {
                self.mmu.write_u8(address, data);
                false
            }
            7 => {
                self.mmu.write_u8(address, data);
                true
            }
            _ => panic!("{}", self.curr_cycle),
        }
    }
    fn izx_write(&mut self, data: u8) -> bool {
        let address = self.get_instr().get_address().unwrap();
        match self.curr_cycle {
			6=>false,
            7 => {
                self.mmu.write_u8(address, data);
                false
            }
            8 => {
                self.mmu.write_u8(address, data);
                true
            }
            _ => panic!("{}", self.curr_cycle),
        }
    }
    fn izy_write(&mut self, data: u8) -> bool {
		let address = self.get_instr().get_address().unwrap();
        match self.curr_cycle {
			6 => false,
            7 => {
                self.mmu.write_u8(address, data);
                false
            }
            8 => {
                self.mmu.write_u8(address, data);
                true
            }
            _ => panic!("{}", self.curr_cycle),
        }
	}
	fn imp_write(&mut self, data: u8) -> bool {
		self.a = data;
		true
	}
    /**
    	* Returns true when done writing
    	*/
    fn handle_addressing_mode_write(&mut self, data: u8) -> bool {
        match self.curr_instruction.as_ref().unwrap().mode {
            AddressingMode::ABS => self.abs_write(data),
            AddressingMode::ZP => self.zp_write(data),
            AddressingMode::REL => self.rel_write(),
            AddressingMode::IND => self.ind_write(),
            AddressingMode::ZPX => self.zpx_write(data),
            AddressingMode::ZPY => self.zpy_write(),
            AddressingMode::ABX => self.abx_write(data),
            AddressingMode::ABY => self.aby_write(data),
            AddressingMode::IZX => self.izx_write(data),
			AddressingMode::IZY => self.izy_write(data),
			AddressingMode::Implicit => self.imp_write(data),
            _ => true, //no write cycles needed
        }
    }

    //instructions:
    fn stp(&mut self) {
        self.stopped = true;
    }
    fn brk(&mut self) {
        match self.curr_cycle {
            2 => {
                self.read_instr();
            }
            3 => {
                self.set_status_flag(StatusFlag::Break, true);
                self.push_pch();
            }
            4 => self.push_pcl(),
            5 => self.push_stack(self.p),
            6 => self.get_pcl(0xFFFE),
            7 => {
                self.get_pch(0xFFFF);
                self.curr_instruction = None;
            }
            _ => unreachable!(),
        }
    }

    fn rti(&mut self) {
        match self.curr_cycle {
            2 => {
                self.read_instr();
            }
            3 => {},
			4 => {
				self.p = self.pop_stack();
				self.set_status_flag(StatusFlag::_Unused, true);
			},
            5 => self.pop_pcl(),
            6 => {
                self.pop_pch();
                self.curr_instruction = None;
            }
            _ => unreachable!(),
        }
    }

    fn rts(&mut self) {
        match self.curr_cycle {
            2 => {
                self.read_instr();
            }
            3 => {},//self.sp += 1,
            4 => self.pop_pcl(),
            5 => self.pop_pch(),
            6 => {
                self.pc += 1;
                self.curr_instruction = None;
            }
            _ => unreachable!(),
        }
    }

    fn pha(&mut self) {
        match self.curr_cycle {
            2 => {
				self.mmu.get_u8(self.pc);
            }
            3 => {
                self.push_stack(self.a);
                self.curr_instruction = None;
            }
            _ => unreachable!(),
        }
    }

    fn php(&mut self) {
        match self.curr_cycle {
            2 => {
				self.mmu.get_u8(self.pc);
            }
            3 => {
				//set break flag before pushing to stack for some reason...
				let data = self.p | (1 << StatusFlag::Break as u8);
                self.push_stack(data);
                self.curr_instruction = None;
            }
            _ => unreachable!(),
        }
    }

    fn pla(&mut self) {
        match self.curr_cycle {
            2 => {
                self.mmu.get_u8(self.pc);
            }
            3 => {
				self.a = self.pop_stack();
				self.set_n_z_flags(self.a);
			}
			4=> self.curr_instruction = None,//already done with pulling from stack in last step
            _ => unreachable!(),
        }
    }

    fn plp(&mut self) {
        match self.curr_cycle {
            2 => {
                self.mmu.get_u8(self.pc);
            }
            3 => {
				//target -> 0b1010_1101 to 0b1110_1111
				//actual -> 0b1010_1101 to 0b1111_1111
				self.p = self.pop_stack();
				self.set_status_flag(StatusFlag::_Unused, true);
				self.set_status_flag(StatusFlag::Break, false);
			}
			4=> self.curr_instruction = None,//already done with pulling from stack in last step
            _ => unreachable!(),
        }
    }

    fn jsr(&mut self) {
        match self.curr_cycle {
            2 => {
                //fetch low address byte and increment pc, store it to put in pcl later
                self.get_instr_mut().addr_low = Some(*self.mmu.get_u8(self.pc));
                self.pc += 1;
            }
            3 => {}
            4 => self.push_pch(),
            5 => self.push_pcl(),
            6 => {
				//set pcl from previously fetched data
				self.get_instr_mut().addr_high = Some(*self.mmu.get_u8(self.pc));
                self.pc = self.get_instr().get_address().unwrap();
                self.curr_instruction = None;
            }
            _ => unreachable!(),
        }
    }

    fn jmp(&mut self) {
		match self.get_instr().mode {
			AddressingMode::ABS => {
				self.handle_addressing_mode_read();
				if self.get_instr().get_address().is_some() {
					self.pc = self.get_instr().get_address().unwrap();
					self.curr_instruction = None;
				}
			}
			AddressingMode::IND => {
				self.handle_addressing_mode_read();
				if self.get_instr().get_address().is_some() {
					self.pc = self.get_instr().get_address().unwrap();
					self.curr_instruction = None;
				}
			}
			_ =>unreachable!(),
		}
    }

    fn lda(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            self.a = instruction.data.unwrap();
            self.set_n_z_flags(self.a);
            self.curr_instruction = None;
        }
    }

    fn ldx(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            self.x = instruction.data.unwrap();
            self.set_n_z_flags(self.x);
            self.curr_instruction = None;
        }
    }

    fn ldy(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            self.y = instruction.data.unwrap();
            self.set_n_z_flags(self.y);
            self.curr_instruction = None;
        }
    }

    fn eor(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            self.a = self.a ^ instruction.data.unwrap();
            self.set_n_z_flags(self.a);
            self.curr_instruction = None;
        }
    }

    fn and(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            self.a = self.a & instruction.data.unwrap();
            self.set_n_z_flags(self.a);
            self.curr_instruction = None;
        }
    }

    fn ora(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            self.a = self.a | instruction.data.unwrap();
            self.set_n_z_flags(self.a);
            self.curr_instruction = None;
        }
    }

    fn adc(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let carry = if self.get_status_flag(StatusFlag::Carry) {
                1
            } else {
                0
            };
			let data = instruction.data.unwrap();
			let result = self.a as u16 + data as u16 + carry as u16;

            self.set_n_z_flags(result as u8);
            self.set_status_flag(StatusFlag::Carry, result > 0xFF);
			self.set_status_flag(StatusFlag::Overflow, (!(self.a as u16 ^ data as u16) & (self.a as u16 ^ result)) & 0x0080 != 0);
			self.a = (result & 0xFF) as u8;
            self.curr_instruction = None;
        }
    }

    fn sbc(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let carry = if self.get_status_flag(StatusFlag::Carry) {
                1
            } else {
                0
            } as u16;
			//flip the bottom 8 bits
			let data = instruction.data.unwrap() as u16 ^ 0x00FF;
			
            let result = self.a as u16 + data as u16 + carry as u16;

            self.set_n_z_flags(result as u8);
            self.set_status_flag(StatusFlag::Carry, result > 0xFF);
			self.set_status_flag(StatusFlag::Overflow, (result ^ self.a as u16) & (result^data) & 0x0080 != 0);
			self.a = (result & 0xFF) as u8;
            self.curr_instruction = None;
        }
    }

    fn cmp(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let data = instruction.data.unwrap() as u16;
            //give ourselves extra room to calculate the sub result
            let result: u16 = (0xFF00 | self.a as u16) - data;

            self.set_n_z_flags((result & 0xFF) as u8);
            self.set_status_flag(StatusFlag::Carry, self.a >= data as u8);
            self.curr_instruction = None;
        }
    }
    fn cpx(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let data = instruction.data.unwrap() as u16;
            //give ourselves extra room to calculate the sub result
            let result: u16 = (0xFF00 | self.x as u16) - data;

            self.set_n_z_flags((result & 0xFF) as u8);
            self.set_status_flag(StatusFlag::Carry, self.x >= data as u8);
            self.curr_instruction = None;
        }
    }
    fn cpy(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let data = instruction.data.unwrap() as u16;
            //give ourselves extra room to calculate the sub result
            let result: u16 = (0xFF00 | self.y as u16) - data;

            self.set_n_z_flags((result & 0xFF) as u8);
            self.set_status_flag(StatusFlag::Carry, self.y >= data as u8);
            self.curr_instruction = None;
        }
    }

    fn bit(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
			let data = instruction.data.unwrap();
            self.set_status_flag(StatusFlag::Negative, data >> 7 == 0x01);
            self.set_status_flag(StatusFlag::Overflow, ((data >> 6) & 0x1) == 0x01);
            self.set_status_flag(StatusFlag::Zero, data & self.a == 0);
            self.curr_instruction = None;
        }
    }

    fn lax(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let data = instruction.data.unwrap();
            self.a = data;
            self.x = data;
            self.set_n_z_flags(data);
            self.curr_instruction = None;
        }
    }

    fn nop(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            self.curr_instruction = None;
        }
    }

    fn asl(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let result = instruction.data.unwrap() << 1;
            if self.handle_addressing_mode_write(result) {
				self.set_status_flag(StatusFlag::Carry, instruction.data.unwrap() >> 7 == 0x1);
				self.set_n_z_flags(result);
                self.curr_instruction = None;
            }
        }
    }

    fn lsr(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let result = instruction.data.unwrap() >> 1;
            if self.handle_addressing_mode_write(result) {
				self.set_status_flag(StatusFlag::Carry, instruction.data.unwrap() & 0x1 == 0x1);
				self.set_n_z_flags(result);
                self.curr_instruction = None;
            }
        }
    }

    fn rol(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let carry = self.get_status_flag(StatusFlag::Carry);
            let mut result = instruction.data.unwrap() << 1;
            if carry {
                result |= 1;
            }
            if self.handle_addressing_mode_write(result) {
				self.set_status_flag(StatusFlag::Carry, instruction.data.unwrap() >> 7 == 0x1);
				self.set_n_z_flags(result);
                self.curr_instruction = None;
            }
        }
    }

    fn ror(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
			//operand 0101_0101
			//target 0010_1010
			//actual 1010_1010
            let carry = self.get_status_flag(StatusFlag::Carry);
			let mut result = instruction.data.unwrap() >> 1;
            if carry {
				result |= 0b1000_0000;
            }
            if self.handle_addressing_mode_write(result) {
				self.set_status_flag(StatusFlag::Carry, instruction.data.unwrap() & 0x1 == 0x1);
				self.set_n_z_flags(result);
                self.curr_instruction = None;
            }
        }
    }

    fn inc(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let result = instruction.data.unwrap() as u16 + 1;
            self.set_n_z_flags(result as u8);
            if self.handle_addressing_mode_write(result as u8) {
                self.curr_instruction = None;
            }
        }
    }

    fn dec(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
			let data = instruction.data.unwrap();
			let result: u8;
			if data == 0x00 {
				result = 0xFF;
			}
			else {
				result = data - 1;
			}
            self.set_n_z_flags(result);
            if self.handle_addressing_mode_write(result) {
                self.curr_instruction = None;
            }
        }
    }

    fn dex(&mut self) {
        if self.x == 0x00 {
			self.x = 0xFF;
		}
		else {
			self.x -= 1;
		}
        self.set_n_z_flags(self.x);
        self.curr_instruction = None;
    }
    fn dey(&mut self) {
		if self.y == 0x00 {
			self.y = 0xFF;
		}
		else {
			self.y -= 1;
		}
        self.set_n_z_flags(self.y);
        self.curr_instruction = None;
    }
    fn inx(&mut self) {
        let result = self.x as u16 + 1;
        self.x = (result & 0xFF) as u8;
        self.set_n_z_flags(self.x);
        self.curr_instruction = None;
    }
    fn iny(&mut self) {
		let result = self.y as u16 + 1;
        self.y = (result & 0xFF) as u8;
        self.set_n_z_flags(self.y);
        self.curr_instruction = None;
    }
    fn tax(&mut self) {
        self.x = self.a;
        self.set_n_z_flags(self.x);
        self.curr_instruction = None;
    }
    fn txa(&mut self) {
        self.a = self.x;
        self.set_n_z_flags(self.a);
        self.curr_instruction = None;
    }
    fn tay(&mut self) {
        self.y = self.a;
        self.set_n_z_flags(self.y);
        self.curr_instruction = None;
    }
    fn tya(&mut self) {
        self.a = self.y;
        self.set_n_z_flags(self.a);
        self.curr_instruction = None;
    }
    fn tsx(&mut self) {
        self.x = self.sp;
        self.set_n_z_flags(self.x);
        self.curr_instruction = None;
    }
    fn txs(&mut self) {
        self.sp = self.x;
        self.curr_instruction = None;
    }

    fn slo(&mut self) {
		//ASL and ORA
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let data = instruction.data.unwrap();
            let result = data << 1;
            self.set_status_flag(StatusFlag::Carry, data >> 7 == 0x1);
            self.a |= result;
            self.set_n_z_flags(self.a);
            if self.handle_addressing_mode_write(result) {
                self.curr_instruction = None;
            }
        }
    }

    fn sre(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let data = instruction.data.unwrap();
            let result = data >> 1;
            self.a ^= result; //todo: not sure if this is right... should it be xored with the data before or after the shift?
            if self.handle_addressing_mode_write(result) {
				self.set_status_flag(StatusFlag::Carry, data & 0x1 == 0x1);
				self.set_n_z_flags(self.a);
                self.curr_instruction = None;
            }
        }
    }

    fn rla(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let data = instruction.data.unwrap();
            let carry = self.get_status_flag(StatusFlag::Carry);
            let mut result = data << 1;
            if carry {
                result |= 1;
            }
            self.a &= result;
            if self.handle_addressing_mode_write(result) {
				self.set_status_flag(StatusFlag::Carry, data >> 7 == 0x1);
				self.set_n_z_flags(result);
                self.curr_instruction = None;
            }
        }
    }

    fn rra(&mut self) {
		//ROR and ADC
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
			let data  = instruction.data.unwrap();
            let mut carry = self.get_status_flag(StatusFlag::Carry);
			let result = data >> 1;
			let ror_result: u8;
			if carry {
				ror_result = result | 0b1000_0000;
			}
			else {
				ror_result = result;
			}
			carry = data & 0x1 == 0x1;//add uses the carry from the ror
            let add_result =
				self.a as u16 + ror_result as u16 + if carry { 1 } else { 0 };
            if self.handle_addressing_mode_write(ror_result) {
				self.a = (add_result & 0xFF) as u8;
				self.set_n_z_flags(add_result as u8);
				self.set_status_flag(StatusFlag::Carry, add_result > 0xFF);
				self.set_status_flag(StatusFlag::Overflow, (!(self.a as u16 ^ data as u16) & (self.a as u16 ^ add_result)) & 0x0080 != 0);
	
                self.curr_instruction = None;
            }
        }
    }

    fn isb(&mut self) {
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let data = instruction.data.unwrap();
			let result = ((data as u16 + 1) & 0xFF) as u8;
			if self.handle_addressing_mode_write(result as u8) {
				//subtract
				let carry = if self.get_status_flag(StatusFlag::Carry) {
					1
				} else {
					0
				} as u16;
				//flip the bottom 8 bits
				let data = instruction.data.unwrap() as u16 ^ 0x00FF;		
				let result = self.a as u16 + data as u16 + carry as u16;
				self.a = (result - 1) as u8;
				self.set_n_z_flags(self.a as u8);
				self.set_status_flag(StatusFlag::Carry, result > 0xFF);
				self.set_status_flag(StatusFlag::Overflow, (result ^ self.a as u16) & (result^data) & 0x0080 != 0);
				
				self.curr_instruction = None;
			}
        }
    }

    fn dcp(&mut self) {
        //decrement with compare
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let data = instruction.data.unwrap();
			let result: u8;
			if data == 0x00 {
				result = 0xFF;
			}
			else {
				result = data - 1;
			}
			self.set_n_z_flags(result);
            if self.handle_addressing_mode_write(result) {
                self.curr_instruction = None;
			}

            //give ourselves extra room to calculate the sub result
            let result: u16 = (0xFF00 | self.a as u16) - result as u16;
            self.set_n_z_flags((result & 0xFF) as u8);
            self.set_status_flag(StatusFlag::Carry, self.a >= data as u8);
        }
    }

    fn alr(&mut self) {
        //AND and LSR
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let data = instruction.data.unwrap();
            self.set_status_flag(StatusFlag::Carry, self.a >> 7 == 0x1);
            self.a = (self.a & data) >> 1;
            self.set_n_z_flags(self.a);
        }
    }
    fn arr(&mut self) {
        //AND and ROR
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let data = instruction.data.unwrap();
            self.set_status_flag(StatusFlag::Carry, self.a >> 7 == 0x1);
            self.a = (self.a & data) >> 1;
            //todo: test how the overflow flag is supposed to be set here, it's really unclear
            self.set_n_z_flags(self.a);
        }
    }
    fn xaa(&mut self) {
        //TXA and AND imm
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let data = instruction.data.unwrap();
            self.a = self.x & data;
            self.set_n_z_flags(self.a);
        }
    }
    fn ahx(&mut self) {
        //store A&X&H into address
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let data = self.a & self.x & ((instruction.get_address().unwrap() >> 8) as u8 + 1);
            self.handle_addressing_mode_write(data);
        }
    }
    fn shx(&mut self) {
        //store X&H into address
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let data = self.x & ((instruction.get_address().unwrap() >> 8) as u8 + 1);
            self.handle_addressing_mode_write(data);
        }
    }
    fn shy(&mut self) {
        //store Y&H into address
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let data = self.y & ((instruction.get_address().unwrap() >> 8) as u8 + 1);
            self.handle_addressing_mode_write(data);
        }
    }
    fn tas(&mut self) {
        //store A&X into S and A&X&H into {adr}
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let data = self.a & self.x & ((instruction.get_address().unwrap() >> 8) as u8 + 1);
            self.sp = self.a & self.x;
            self.handle_addressing_mode_write(data);
        }
    }
    fn las(&mut self) {
        //stores {adr}&S into A, X and S
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let data = instruction.data.unwrap() & self.sp;
            self.a = data;
            self.x = data;
            self.y = data;
            self.curr_instruction = None;
        }
    }
    fn axs(&mut self) {
        //CMP and DEX
        //X:=A&X-#{imm}
        self.handle_addressing_mode_read();
        let instruction = self.get_instr();
        if instruction.data.is_some() {
            let data = instruction.data.unwrap();
            let result = self.a & self.x - data;
            self.x = result;
            self.set_status_flag(StatusFlag::Carry, self.a & self.x >= data as u8);
            self.set_n_z_flags(result);
        }
    }

    fn sta(&mut self) {
        self.handle_addressing_mode_read();
        if self.get_instr().data.is_some() {
			self.mmu.write_u8(self.get_instr().get_address().unwrap(), self.a);
			if !((	(self.get_instr().mode == AddressingMode::IZY && self.curr_cycle == 5) ||
					(self.get_instr().mode == AddressingMode::ABY && self.curr_cycle == 4) ||
					(self.get_instr().mode == AddressingMode::ABX && self.curr_cycle == 4))
					 && !self.get_instr().fix_high_byte) {
				self.curr_instruction = None;
			}
			//else, take an extra cycle... the read being done for izy writes isn't necesarily indicative of it being done... for writes (sta, stx, etc.) we should actually check if the address field has been populated, then take the extra cycle to do the write
        }
    }

    fn stx(&mut self) {
        self.handle_addressing_mode_read();
        if self.get_instr().data.is_some() {
			self.mmu.write_u8(self.get_instr().get_address().unwrap(), self.x);
			self.curr_instruction = None;
        }
    }

    fn sty(&mut self) {
        self.handle_addressing_mode_read();
        if self.get_instr().data.is_some() {
			self.mmu.write_u8(self.get_instr().get_address().unwrap(), self.y);
			self.curr_instruction = None;
        }
    }

    fn sax(&mut self) {
        self.handle_addressing_mode_read();
        if self.get_instr().data.is_some() {
			self.mmu.write_u8(self.get_instr().get_address().unwrap(), self.a&self.x);
			self.curr_instruction = None;
        }
    }

    fn anc(&mut self) {
        self.handle_addressing_mode_read();
        if self.get_instr().data.is_some() {
            let data = self.get_instr().data.unwrap();
            self.a &= data;
            self.set_status_flag(StatusFlag::Carry, data >> 7 == 0x1); //set carry as if a ROL happened
            self.set_n_z_flags(self.a);
            self.curr_instruction = None;
        }
    }

    //for branch instructions, the target is PC + signed offset immediate
    fn handle_branch(&mut self, offset: u8) {
        match self.curr_cycle {
			2 => {
				//nothing to do... we are here because we have the data already
			}
            3 => {
				//if the low byte of the pc + the offset is different, then we need to fix the high byte next cycle
				let offset_is_negative = offset >> 7 == 0x1;
				let mut fix_high_byte = false;
				let new_address_low: u16;
				if offset_is_negative {
					let pc_low = (self.pc & 0x00FF) | 0xFF00;//set the upper byte to FF so that we can detect a borrow
					new_address_low = (pc_low as i16 + (offset as u16 | 0xFF00) as i16) as u16;
				}
				else{
					let pc_low = self.pc & 0xFF;
					new_address_low = pc_low + offset as u16;
					if new_address_low > 0xFF {
						fix_high_byte = true;
					}
				}

				self.get_instr_mut().fix_high_byte = fix_high_byte;
				self.pc = new_address_low & 0x00FF | self.pc & 0xFF00;
				if !fix_high_byte {
					//we're done
					self.curr_instruction = None;
				}
            }
            4 => {
				//need to fix PCH
				let offset_is_negative = offset >> 7 == 0x1;
				if !offset_is_negative {
					self.pc += 0x0100;
				}
				self.curr_instruction = None;
            }
            _ => panic!("{}", self.curr_cycle),
        };
    }

    fn bpl(&mut self) {
        self.handle_addressing_mode_read();
        if self.get_instr().data.is_some() {
            let data = self.get_instr().data.unwrap();
            if !self.get_status_flag(StatusFlag::Negative) {
                self.handle_branch(data);
            } else {
                self.curr_instruction = None;
            }
        }
    }
    fn bmi(&mut self) {
        self.handle_addressing_mode_read();
        if self.get_instr().data.is_some() {
            let data = self.get_instr().data.unwrap();
            if self.get_status_flag(StatusFlag::Negative) {
                self.handle_branch(data);
            } else {
                self.curr_instruction = None;
            }
        }
    }
    fn bvc(&mut self) {
        self.handle_addressing_mode_read();
        if self.get_instr().data.is_some() {
            let data = self.get_instr().data.unwrap();
            if !self.get_status_flag(StatusFlag::Overflow) {
                self.handle_branch(data);
            } else {
                self.curr_instruction = None;
            }
        }
    }
    fn bvs(&mut self) {
        self.handle_addressing_mode_read();
        if self.get_instr().data.is_some() {
            let data = self.get_instr().data.unwrap();
            if self.get_status_flag(StatusFlag::Overflow) {
                self.handle_branch(data);
            } else {
                self.curr_instruction = None;
            }
        }
    }
    fn bcc(&mut self) {
        self.handle_addressing_mode_read();
        if self.get_instr().data.is_some() {
            let data = self.get_instr().data.unwrap();
            if !self.get_status_flag(StatusFlag::Carry) {
                self.handle_branch(data);
            } else {
                self.curr_instruction = None;
            }
        }
    }
    fn bcs(&mut self) {
        self.handle_addressing_mode_read();
        if self.get_instr().data.is_some() {
            let data = self.get_instr().data.unwrap();
            if self.get_status_flag(StatusFlag::Carry) {
                self.handle_branch(data);
            } else {
                self.curr_instruction = None;
            }
        }
    }
    fn bne(&mut self) {
        self.handle_addressing_mode_read();
        if self.get_instr().data.is_some() {
            let data = self.get_instr().data.unwrap();
            if !self.get_status_flag(StatusFlag::Zero) {
                self.handle_branch(data);
            } else {
                self.curr_instruction = None;
            }
        }
    }
    fn beq(&mut self) {
        self.handle_addressing_mode_read();
        if self.get_instr().data.is_some() {
            let data = self.get_instr().data.unwrap();
            if self.get_status_flag(StatusFlag::Zero) {
                self.handle_branch(data);
            } else {
                self.curr_instruction = None;
            }
        }
    }

    fn clc(&mut self) {
		self.set_status_flag(StatusFlag::Carry, false);
		self.curr_instruction = None;
    }
    fn sec(&mut self) {
		self.set_status_flag(StatusFlag::Carry, true);
		self.curr_instruction = None;
    }
    fn cld(&mut self) {
        self.set_status_flag(StatusFlag::Decimal, false);
		self.curr_instruction = None;
    }
    fn sed(&mut self) {
        self.set_status_flag(StatusFlag::Decimal, true);
		self.curr_instruction = None;
    }
    fn cli(&mut self) {
        self.set_status_flag(StatusFlag::InterruptDisable, false);
		self.curr_instruction = None;
    }
    fn sei(&mut self) {
        self.set_status_flag(StatusFlag::InterruptDisable, true);
		self.curr_instruction = None;
    }
    fn clv(&mut self) {
        self.set_status_flag(StatusFlag::Overflow, false);
		self.curr_instruction = None;
    }
}

pub struct MMU {
    pub ram: Vec<u8>,
}
impl MMU {
    pub fn get_u8(&self, address: u16) -> &u8 {
        //println!("Reading {:x?} from {:x?}", self.ram[address as usize], address);
        //todo: memory mapping
        &self.ram[address as usize]
    }

    pub fn write_u8(&mut self, address: u16, data: u8) {
        //println!("Writing {:x?} to {:x?}", data, address);
        self.ram[address as usize] = data;
    }
}

#[cfg(test)]
mod tests {
    use crate::cpu::StatusFlag;
    use crate::CPU;
    #[test]
    fn cpu() {
        let mut test_cpu = CPU::new();
        //test status flags:
        assert_eq!(test_cpu.get_status_flag(StatusFlag::Break), false);
        test_cpu.set_status_flag(StatusFlag::Break, true);
        assert_eq!(test_cpu.get_status_flag(StatusFlag::Break), true);
        test_cpu.set_status_flag(StatusFlag::Carry, true);
        assert_eq!(test_cpu.get_status_flag(StatusFlag::Carry), true);
        test_cpu.set_status_flag(StatusFlag::Break, false);
        assert_eq!(test_cpu.get_status_flag(StatusFlag::Break), false);
        test_cpu.p = 0;

        test_cpu.push_stack(0x01);
        test_cpu.push_stack(0x02);
        //the assumption is that when popping from stack, the sp is already decremented before the first pop (if multiple)
        test_cpu.sp += 1;
        assert_eq!(test_cpu.pop_stack(), 0x02);
        assert_eq!(test_cpu.pop_stack(), 0x01);
    }

    #[test]
    fn interrupt() {
        let mut test_cpu = CPU::new();
        test_cpu.pc = 0xBEEF;
        test_cpu.p = 0b0010_0110;
        test_cpu.curr_instruction = Some(CPU::decode_instr(0x00, 0, 0)); //brk
        test_cpu.curr_cycle = 2;
        for _ in 2..8 {
            test_cpu.step();
        }
        assert!(test_cpu.curr_instruction.is_none());
        test_cpu.curr_instruction = Some(CPU::decode_instr(0x40, 0, 0)); //rti
        test_cpu.curr_cycle = 2;
        for _ in 2..7 {
            test_cpu.step();
        }
        assert!(test_cpu.curr_instruction.is_none());
        assert_eq!(test_cpu.pc, 0xBEEF + 1);
        assert_eq!(test_cpu.p, 0b0011_0110); //b flag is set after a break!
    }

    #[test]
    fn add() {
        //test base adding
        let mut test_cpu = CPU::new();
        test_cpu.set_status_flag(StatusFlag::Carry, true);
        test_cpu.a = 1;
        test_cpu.mmu.write_u8(0x0001, 0x00);
        test_cpu.mmu.write_u8(0x0002, 0x10);
        test_cpu.mmu.write_u8(0x1000, 1);
        test_cpu.curr_instruction = Some(CPU::decode_instr(0x6D, 0, 0)); //adc, ABS
        test_cpu.pc = 0x0001;
        while !test_cpu.curr_instruction.is_none() {
            test_cpu.step();
        }
        assert_eq!(test_cpu.a, 3);
        assert_eq!(test_cpu.p, 0b0000_0000);

        //test overflow
        let mut test_cpu = CPU::new();
        test_cpu.a = 123;
        test_cpu.mmu.write_u8(0x0001, 0x00);
        test_cpu.mmu.write_u8(0x0002, 0x10);
        test_cpu.mmu.write_u8(0x1000, 45);
        test_cpu.curr_instruction = Some(CPU::decode_instr(0x6D,0,0)); //adc, ABS
        test_cpu.pc = 0x0001;
        while !test_cpu.curr_instruction.is_none() {
            test_cpu.step();
        }
        assert_eq!(test_cpu.a, 168);
        assert_eq!(test_cpu.p, 0b1100_0000);

        //test overflow and carry
        let mut test_cpu = CPU::new();
        test_cpu.a = 128;
        test_cpu.mmu.write_u8(0x0001, 0x00);
        test_cpu.mmu.write_u8(0x0002, 0x10);
        test_cpu.mmu.write_u8(0x1000, 128);
        test_cpu.curr_instruction = Some(CPU::decode_instr(0x6D,0,0)); //adc, ABS
        test_cpu.pc = 0x0001;
        while !test_cpu.curr_instruction.is_none() {
            test_cpu.step();
        }
        assert_eq!(test_cpu.a, 0);
        assert_eq!(test_cpu.p, 0b0100_0011);
    }
}
