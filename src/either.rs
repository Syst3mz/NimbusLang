#[derive(Debug, Clone)]
pub enum Either<T1, T2> {
    A(T1),
    B(T2)
}