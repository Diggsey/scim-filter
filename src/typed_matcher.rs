use rust_decimal::Decimal;

use crate::error::Error;
use crate::parser::{
    scim_filter_parser, AttrExpData, AttrPath, CompValue, CompareOp, Filter, LogExpData, ValFilter,
    ValuePathData,
};

pub enum ScimSingleValue<'a> {
    String(&'a str),
    CaseExactString(&'a str),
    Boolean(bool),
    Number(Decimal),
    Complex,
}

pub enum ScimValue<'a> {
    Single(Option<ScimSingleValue<'a>>),
    Multi(Vec<Option<ScimSingleValue<'a>>>),
}

#[derive(Default, Copy, Clone)]
struct Context<'a> {
    parent_attr: Option<&'a AttrPath>,
    index: Option<usize>,
}

impl Context<'_> {
    fn extract_value<'a, T: ScimResource>(
        &self,
        attr: &AttrPath,
        resource: &'a T,
    ) -> ScimValue<'a> {
        let mut value = if let Some(parent_attr) = self.parent_attr {
            resource.extract_value(&AttrPath::new((
                None,
                parent_attr.attr_name().clone(),
                Some(attr.attr_name().clone()),
            )))
        } else {
            resource.extract_value(attr)
        };
        if let Some(index) = self.index {
            value = ScimValue::Single(value.into_iter().nth(index).flatten());
        }
        value
    }
}

impl<'a> IntoIterator for ScimValue<'a> {
    type Item = Option<ScimSingleValue<'a>>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            ScimValue::Single(value) => vec![value].into_iter(),
            ScimValue::Multi(values) => values.into_iter(),
        }
    }
}

pub trait ScimResource {
    fn extract_value<'a>(&'a self, path: &AttrPath) -> ScimValue<'a>;
}

type MatcherResult<T> = Result<T, Error>;

pub fn scim_typed_filter<T>(
    input: &str,
    resources: impl IntoIterator<Item = T>,
) -> Result<Vec<T>, Error>
where
    T: ScimResource,
{
    let filter_expression = scim_filter_parser(input)?;

    resources.into_iter().try_fold(vec![], |mut acc, resource| {
        match filter_expression.typed_match(Default::default(), &resource) {
            Ok(true) => {
                acc.push(resource);
                Ok(acc)
            }
            Ok(false) => Ok(acc),
            Err(e) => Err(e),
        }
    })
}

impl Filter<'_> {
    fn typed_match<T: ScimResource>(&self, context: Context, resource: &T) -> MatcherResult<bool> {
        match self {
            Filter::AttrExp(attr_expr_data) => attr_expr_data.typed_match(context, resource),
            Filter::LogExp(log_exp_data) => log_exp_data.typed_match(context, resource),
            Filter::ValuePath(value_path_data) => value_path_data.typed_match(resource),
            Filter::Sub(is_not, filter) => {
                filter.typed_match(context, resource).map(|filter_result| {
                    if *is_not {
                        !filter_result
                    } else {
                        filter_result
                    }
                })
            }
        }
    }
}

fn compare_strings(op: &CompareOp, left: &str, right: &str) -> bool {
    match op {
        CompareOp::Equal => left == right,
        CompareOp::NotEqual => left != right,
        CompareOp::Contains => left.contains(right),
        CompareOp::StartsWith => left.starts_with(right),
        CompareOp::EndsWith => left.ends_with(right),
        CompareOp::GreaterThan => left > right,
        CompareOp::GreaterThanOrEqual => left >= right,
        CompareOp::LessThan => left < right,
        CompareOp::LessThanOrEqual => left <= right,
    }
}

fn compare_decimals(op: &CompareOp, left: &Decimal, right: &Decimal) -> bool {
    match op {
        CompareOp::Equal => left == right,
        CompareOp::NotEqual => left != right,
        CompareOp::Contains => left == right,
        CompareOp::StartsWith => left == right,
        CompareOp::EndsWith => left == right,
        CompareOp::GreaterThan => left > right,
        CompareOp::GreaterThanOrEqual => left >= right,
        CompareOp::LessThan => left < right,
        CompareOp::LessThanOrEqual => left <= right,
    }
}

fn compare_bools(op: &CompareOp, left: bool, right: bool) -> bool {
    match op {
        CompareOp::Equal => left == right,
        CompareOp::NotEqual => left != right,
        CompareOp::Contains => left == right,
        CompareOp::StartsWith => left == right,
        CompareOp::EndsWith => left == right,
        CompareOp::GreaterThan => left && !right,
        CompareOp::GreaterThanOrEqual => left || !right,
        CompareOp::LessThan => !left && right,
        CompareOp::LessThanOrEqual => !left || right,
    }
}

impl AttrExpData<'_> {
    fn typed_match<T: ScimResource>(&self, context: Context, resource: &T) -> MatcherResult<bool> {
        match self {
            AttrExpData::Present(attr_path) => Ok(context
                .extract_value(attr_path, resource)
                .into_iter()
                .any(|v| v.is_some())),
            AttrExpData::Compare(attr_path, compare_op, comp_value) => {
                let resource_value = context.extract_value(attr_path, resource);
                for value in resource_value {
                    match (value, comp_value) {
                        (Some(ScimSingleValue::String(value)), CompValue::String(comp_value)) => {
                            if compare_strings(
                                compare_op,
                                &value.to_ascii_lowercase(),
                                &comp_value.to_ascii_lowercase(),
                            ) {
                                return Ok(true);
                            }
                        }
                        (
                            Some(ScimSingleValue::CaseExactString(value)),
                            CompValue::String(comp_value),
                        ) => {
                            if compare_strings(compare_op, value, comp_value) {
                                return Ok(true);
                            }
                        }
                        (Some(ScimSingleValue::Boolean(value)), CompValue::True) => {
                            if compare_bools(compare_op, value, true) {
                                return Ok(true);
                            }
                        }
                        (Some(ScimSingleValue::Boolean(value)), CompValue::False) => {
                            if compare_bools(compare_op, value, false) {
                                return Ok(true);
                            }
                        }
                        (Some(ScimSingleValue::Number(value)), CompValue::Number(comp_value)) => {
                            if compare_decimals(compare_op, &value, comp_value) {
                                return Ok(true);
                            }
                        }
                        (None, CompValue::Null) => {
                            if compare_bools(compare_op, false, false) {
                                return Ok(true);
                            }
                        }
                        (None, _) => {
                            if compare_bools(compare_op, false, true) {
                                return Ok(true);
                            }
                        }
                        (_, CompValue::Null) => {
                            if compare_bools(compare_op, true, false) {
                                return Ok(true);
                            }
                        }
                        _ => {}
                    }
                }
                Ok(false)
            }
        }
    }
}

impl LogExpData<'_> {
    fn typed_match<T: ScimResource>(&self, context: Context, resource: &T) -> MatcherResult<bool> {
        // check if the left side is a match
        let left_match = self.left.typed_match(context, resource)?;

        // if it is, and the operator is an or, no need to check for the right side
        if left_match && self.log_exp_operator.is_or() {
            Ok(true)
        } else if (left_match && self.log_exp_operator.is_and())
            || (!left_match && self.log_exp_operator.is_or())
        {
            // if it's an and operator, or if it's an or and the left don't match, I check for the right side
            self.right.typed_match(context, resource)
        } else {
            Ok(false)
        }
    }
}

impl ValuePathData<'_> {
    fn typed_match<T: ScimResource>(&self, resource: &T) -> MatcherResult<bool> {
        let count = resource.extract_value(self.attr_path()).into_iter().count();
        for index in 0..count {
            if self.val_filter().typed_match(
                Context {
                    parent_attr: Some(self.attr_path()),
                    index: Some(index),
                },
                resource,
            )? {
                return Ok(true);
            }
        }
        Ok(false)
    }
}

impl ValFilter<'_> {
    fn typed_match<T: ScimResource>(&self, context: Context, resource: &T) -> MatcherResult<bool> {
        match self {
            ValFilter::AttrExp(attr_exp_data) => attr_exp_data.typed_match(context, resource),
            ValFilter::LogExp(log_exp_data) => log_exp_data.typed_match(context, resource),
            ValFilter::SubFilter(is_not, sub_filter) => sub_filter
                .typed_match(context, resource)
                .map(|sub_filter_result| {
                    if *is_not {
                        !sub_filter_result
                    } else {
                        sub_filter_result
                    }
                }),
        }
    }
}
