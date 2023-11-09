#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct SourceInfo {
    pub start: usize,
    pub length: usize,
}

impl SourceInfo {
    pub fn span(&self, other: SourceInfo) -> SourceInfo {
        let end = other.start + other.length;
        SourceInfo {
            start: self.start,
            length: end - self.start,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn span() {
        let left = SourceInfo {
            start: 0,
            length: 1,
        };
        let right = SourceInfo {
            start: 10,
            length: 2,
        };
        assert_eq!(
            left.span(right),
            SourceInfo {
                start: 0,
                length: 12
            }
        )
    }
}
