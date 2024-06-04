use std::mem::swap;
// This was the wrong tool, but keeping it in case it is ever needed again.
pub struct Dsu { pub e: Vec<i64> }
impl Dsu {
	pub fn new(n: usize) -> Self { Self { e: vec![-1; n] } }
	pub fn same(&mut self, a: usize, b: usize) -> bool {
		return self.find(a) == self.find(b);
	}
	pub fn size(&mut self, x: usize) -> usize {
		let pos = self.find(x);
		return -self.e[pos] as usize;
	}
	pub fn find(&mut self, x: usize) -> usize { 
        if self.e[x] < 0 { x }
        else {
            self.e[x] = self.find(self.e[x] as usize) as i64;
            self.e[x] as usize
        }
    }
	pub fn join(&mut self, a: usize, b: usize) -> bool {
		let (mut a, mut b) = (self.find(a), self.find(b));
		if a == b { return false }
		if self.e[a] > self.e[b] { swap(&mut a, &mut b) }
		self.e[a] += self.e[b];
        self.e[b] = a as i64;
		return true;
	}
}