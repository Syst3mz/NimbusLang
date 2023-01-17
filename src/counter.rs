#[derive(Debug, Copy, Clone)]
pub struct Counter {
    count: usize
}



impl Counter {
    pub fn new() -> Self {
        Self { count: 0 }
    }
    pub fn next(&mut self) -> usize {
        self.count += 1;
        return self.count
    }

    pub fn count<T>(&mut self, what: T) -> Counted<T> {
        Counted::new(what, self.next())
    }
}

#[derive(Debug)]
pub struct Counted<T> {
    pub inner: T,
    pub count: usize
}

impl<T> Counted<T> {
    pub fn new(inner: T, count: usize) -> Self {
        Self { inner, count }
    }
}