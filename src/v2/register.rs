use std::marker::PhantomData;
use std::ops::RangeInclusive;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Data {
    #[default]
    D0 = 0,
    D1,
    D2,
    D3,
    D4,
    D5,
    D6,
    D7,
}

impl From<usize> for Data {
    fn from(value: usize) -> Data {
        use Data::*;
        match value {
            0 => D0,
            1 => D1,
            2 => D2,
            3 => D3,
            4 => D4,
            5 => D5,
            6 => D6,
            7 => D7,
            _ => unimplemented!(),
        }
    }
}

impl From<Data> for usize {
    fn from(data: Data) -> Self {
        data as usize
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Addr {
    #[default]
    A0 = 0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
}

pub const FP: Addr = Addr::A6;
pub const SP: Addr = Addr::A7;

impl From<usize> for Addr {
    fn from(value: usize) -> Self {
        use Addr::*;
        match value {
            0 => A0,
            1 => A1,
            2 => A2,
            3 => A3,
            4 => A4,
            5 => A5,
            6 => A6,
            7 => A7,
            _ => unimplemented!(),
        }
    }
}

impl From<Addr> for usize {
    fn from(addr: Addr) -> Self {
        addr as usize
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum RegisterUse {
    Free,
    #[default]
    NeverUsed,
    LastUsed(usize),
}

#[derive(Debug, Default)]
pub struct RegisterAllocator<R> {
    rs: [RegisterUse; 8],
    counter: usize,
    phantom: PhantomData<R>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Response<R> {
    pub register: R,
    pub spilled: bool,
}

impl<R: From<usize> + Into<usize> + Copy> RegisterAllocator<R> {
    pub fn new() -> Self {
        Self {
            rs: [RegisterUse::NeverUsed; 8],
            counter: 0,
            phantom: PhantomData,
        }
    }
    pub fn mark(&mut self, r: R) {
        match self.rs[r.into()] {
            RegisterUse::NeverUsed | RegisterUse::Free => panic!("marked unused register"),
            RegisterUse::LastUsed(_) => {}
        };
        self.set_used(r.into());
    }
    fn set_used(&mut self, r: usize) {
        self.counter += 1;
        self.rs[r] = RegisterUse::LastUsed(self.counter);
    }
    pub fn in_use(&self, r: R) -> bool {
        match self.rs[r.into()] {
            RegisterUse::LastUsed(_) => true,
            _ => false,
        }
    }
    pub fn free(&mut self, r: R) {
        self.rs[r.into()] = RegisterUse::Free
    }
    pub fn take(&mut self, r: R) -> bool {
        match self.rs[r.into()] {
            RegisterUse::LastUsed(_) => {
                self.set_used(r.into());
                true
            }
            _ => {
                self.set_used(r.into());
                false
            }
        }
    }
    // order Free < NeverUsed < LastUsed(n) < LastUsed(n + m)
    pub fn take_in_range(&mut self, range: RangeInclusive<usize>) -> Response<R> {
        assert!(!range.is_empty());
        let mut min = (*range.start(), self.rs[*range.start()]);

        for i in range {
            let r = self.rs[i];
            if r < min.1 {
                min = (i, r)
            }
        }
        self.set_used(min.0);
        Response {
            register: R::from(min.0),
            spilled: match min.1 {
                RegisterUse::LastUsed(_) => true,
                _ => false,
            },
        }
    }
    pub fn ever_used(&mut self) -> Vec<R> {
        let mut out = Vec::new();
        for (i, r) in self.rs.iter().enumerate() {
            match r {
                RegisterUse::LastUsed(_) | RegisterUse::Free => {
                    out.push(i.into());
                }
                RegisterUse::NeverUsed => {}
            }
        }
        out
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn take_free_used() {
        let mut a = RegisterAllocator::new();
        let is_spilled = a.take(Data::D2);
        assert!(!is_spilled);
        assert!(a.in_use(Data::D2));

        let is_spilled = a.take(Data::D2);
        assert!(is_spilled);
        assert!(a.in_use(Data::D2));

        a.free(Data::D2);
        assert!(!a.in_use(Data::D2));

        assert_eq!(a.ever_used(), vec![Data::D2]);
    }

    #[test]
    fn take_in_range() {
        let mut a: RegisterAllocator<Data> = RegisterAllocator::new();
        // take never used registers
        let r = a.take_in_range(0..=1);
        assert_eq!(r.register, Data::D0);
        assert_eq!(r.spilled, false);
        let r = a.take_in_range(0..=1);
        assert_eq!(r.register, Data::D1);
        assert_eq!(r.spilled, false);
        // spill least recently used register
        let r = a.take_in_range(0..=1);
        assert_eq!(r.register, Data::D0);
        assert_eq!(r.spilled, true);
        a.mark(Data::D1);
        let r = a.take_in_range(0..=1);
        assert_eq!(r.register, Data::D0);
        assert_eq!(r.spilled, true);

        // prefer freed registers over never used
        a.free(Data::D1);
        let r = a.take_in_range(0..=2);
        assert_eq!(r.register, Data::D1);
        assert_eq!(r.spilled, false);
        let r = a.take_in_range(0..=2);
        assert_eq!(r.register, Data::D2);
        assert_eq!(r.spilled, false);
    }
}
