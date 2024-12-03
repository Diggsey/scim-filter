pub use error::Error;
pub use matcher::scim_filter;
pub use rust_decimal::Decimal;
pub use typed_matcher::{scim_typed_filter, ScimResource, ScimSingleValue, ScimValue};

mod error;
mod matcher;
pub mod parser;
mod typed_matcher;
