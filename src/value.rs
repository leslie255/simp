use std::{fmt::Display, slice, ops::{Try, FromResidual, ControlFlow}};

pub use cranelift::prelude::Value as ClifValue;

/// A value in SIMP lang, could contain zero, one, or more clif values.
/// Additionally, the `Never` variant which indicates a value will never be returned from an
/// expression, such as `break` and `continue`. The `Never` type can be coerced into any type.
/// `Value` also implements `Try` trait in order to return `Never` if `self` is `Never`
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Value {
    Empty,
    /// Uses an array internally in order for `Value::slice` to work
    Single([ClifValue; 1]),
    Tuple(Vec<ClifValue>),
    /// Value of a terminating expression, such as `break` and `continue`
    Never,
}

impl Value {
    /// Returns `true` if the value is [`Empty`].
    ///
    /// [`Empty`]: Value::Empty
    #[allow(dead_code)]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }

    pub fn as_single(&self) -> Option<ClifValue> {
        if let &Self::Single(v) = self {
            Some(v[0])
        } else {
            None
        }
    }

    pub fn expect_single(&self) -> ClifValue {
        match self.as_single() {
            Some(val) => val,
            None => {
                panic!(
                    "Expects a singular value, but {} is provided",
                    self.display()
                );
            }
        }
    }

    #[allow(dead_code)]
    pub fn as_tuple(&self) -> Option<&Vec<ClifValue>> {
        if let Self::Tuple(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_slice(&self) -> &[ClifValue] {
        match self {
            Self::Empty => &[],
            Self::Single(val) => val.as_slice(),
            Self::Tuple(fields) => &fields,
            Self::Never => &[],
        }
    }

    pub fn values(&self) -> Values {
        match self {
            Self::Empty => Values::Empty,
            &Self::Single(val) => Values::Singular(val[0]),
            Self::Tuple(vals) => Values::Tuple(vals.iter()),
            Self::Never => Values::Empty,
        }
    }

    /// Returns `true` if the value is [`Single`].
    ///
    /// [`Single`]: Value::Single
    #[allow(dead_code)]
    #[must_use]
    pub fn is_single(&self) -> bool {
        matches!(self, Self::Single(..))
    }

    /// Returns whether or not it's valid for `other` to be assigned to `self`.
    /// - `Never` can be coerced to any types
    /// - `Tuple`s of 1 value can be coerced to `Single`
    /// - `Tuple`s with 0 values can be coerced to `Empty`
    pub fn matches_val_type(&self, other: &Self) -> bool {
        match (self, other) {
            (_, Self::Never)
            | (Self::Never, _)
            | (Self::Empty, Self::Empty)
            | (Self::Single(..), Self::Single(..)) => true,
            (Self::Empty, Self::Tuple(vals)) if vals.is_empty() => true,
            (Self::Tuple(vals), Self::Empty) if vals.is_empty() => true,
            (Self::Single(..), Self::Tuple(vals)) if vals.len() == 1 => true,
            (Self::Tuple(vals), Self::Single(..)) if vals.len() == 1 => true,
            (Self::Tuple(l), Self::Tuple(r)) if l.len() == r.len() => true,
            _ => false,
        }
    }

    /// Checks whether or not it's valid for value of type `self` to be assigned to expression of
    /// length `len`.
    pub fn can_be_assigned_to_expr_of_len(&self, len: usize) -> bool {
        match (self, len) {
            (Value::Empty, 0) => true,
            (Value::Single(..), 1) => true,
            (Value::Tuple(vals), len) => vals.len() == len,
            (Value::Never, _) => false, // never can never be assigned
            _ => false,
        }
    }

    /// Returns a temporary `ValueDisplay` object for printing a description of SIMP values,
    /// used in error messages.
    /// Formats to
    /// - `"nothing"` for `Value::Empty`
    /// - `"a single integer"` for `Value::Single(..)`
    /// - `"a tuple of {len} fields"` for `Value::Tuple(len)`
    pub fn display(&self) -> ValueDisplay {
        match self {
            Self::Empty => ValueDisplay::Empty,
            Self::Single(..) => ValueDisplay::Single,
            Self::Tuple(vals) => ValueDisplay::Tuple(vals.len()),
            Self::Never => ValueDisplay::Never,
        }
    }

    /// Returns `true` if the value is [`Never`].
    ///
    /// [`Never`]: Value::Never
    #[must_use]
    pub fn is_never(&self) -> bool {
        matches!(self, Self::Never)
    }

    #[must_use]
    pub fn ty(&self) -> ValueType {
        match self {
            Value::Empty => ValueType::Empty,
            Value::Single(_) => ValueType::Single,
            Value::Tuple(vals) => ValueType::Tuple(vals.len()),
            Value::Never => ValueType::Never,
        }
    }

    /// Returns `Never` if `self` is `Never`, otherwise returns `Empty`
    pub fn consume(self) -> Self {
        match self {
            Self::Never => Self::Never,
            _ => Self::Empty,
        }
    }
}

impl Try for Value {
    type Output = Value;

    type Residual = Value;

    fn from_output(output: Self::Output) -> Self {
        output
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            Value::Never => ControlFlow::Break(Value::Never),
            val => ControlFlow::Continue(val),
        }
    }
}

impl FromResidual for Value {
    fn from_residual(residual: <Self as Try>::Residual) -> Self {
        residual
    }
}

/// A hollow value with only the type
#[derive(Debug, Clone, Copy)]
pub enum ValueType {
    Empty,
    Single,
    Tuple(usize),
    Never,
}

impl ValueType {
    /// Returns whether or not it's valid for `other` to be assigned to `self`.
    /// - `Never` can be coerced to any types
    /// - `Tuple`s of 1 value can be coerced to `Single`
    /// - `Tuple`s with 0 values can be coerced to `Empty`
    pub fn matches(self, other: Self) -> bool {
        match (self, other) {
            (_, Self::Never)
            | (Self::Never, _)
            | (Self::Empty, Self::Empty)
            | (Self::Single, Self::Single) => true,
            (Self::Empty, Self::Tuple(0)) => true,
            (Self::Tuple(0), Self::Empty) => true,
            (Self::Single, Self::Tuple(1)) => true,
            (Self::Tuple(1), Self::Single) => true,
            (Self::Tuple(l), Self::Tuple(r)) if l == r => true,
            _ => false,
        }
    }
}

/// An iterator that iterates through the `ClifValue`s contained in a SIMP `Value`.
/// Can be created by `simp_val.values()`.
#[derive(Debug, Clone)]
pub enum Values<'short> {
    Empty,
    Singular(ClifValue),
    Tuple(slice::Iter<'short, ClifValue>),
}

impl Iterator for Values<'_> {
    type Item = ClifValue;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Empty => None,
            Self::Singular(val) => {
                let val = *val;
                *self = Self::Empty;
                Some(val)
            }
            Self::Tuple(iter) => iter.next().copied(),
        }
    }
}

/// A temporary type for printing a description of SIMP Value, used in error messages.
/// Can be created by `simp_val.description()`.
/// Formats to
/// - `"nothing"` for `Value::Empty`
/// - `"a single integer"` for `Value::Single(..)`
/// - `"a tuple of {len} fields"` for `Value::Tuple(len)`
/// - `"a value that would never be obtained"` for `Value::Never`
#[derive(Debug, Clone, Copy)]
pub enum ValueDisplay {
    Empty,
    Single,
    Tuple(usize),
    Never,
}

impl Display for ValueDisplay {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueDisplay::Empty => write!(f, "nothing"),
            ValueDisplay::Single => write!(f, "a single integer"),
            ValueDisplay::Tuple(len) => write!(f, "a tuple of {len} fields"),
            ValueDisplay::Never => write!(f, "a value that would never be obtained"),
        }
    }
}

impl From<ClifValue> for Value {
    fn from(val: ClifValue) -> Self {
        Self::Single([val])
    }
}

impl From<Vec<ClifValue>> for Value {
    fn from(vals: Vec<ClifValue>) -> Self {
        Self::Tuple(vals)
    }
}
