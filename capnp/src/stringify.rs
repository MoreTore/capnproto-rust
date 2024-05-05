use crate::dynamic_value;
use core::fmt::{self, Formatter};

#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct Indent {
    /// None means everything gets printed on a single line.
    indent: Option<usize>,
}

impl Indent {
    fn no_indent() -> Self {
        Self { indent: None }
    }

    fn enabled() -> Self {
        Self { indent: Some(0) }
    }

    fn next(self) -> Self {
        match self.indent {
            None => self,
            Some(x) => Self {
                indent: Some(x + 1),
            },
        }
    }

    fn next_with_num(self, num: usize) -> Self {
        match self.indent {
            None => self,
            Some(x) => Self {
                indent: Some(x + num),
            },
        }
    }

    fn maybe_newline(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        match self.indent {
            None => Ok(()),
            Some(indent) => {
                formatter.write_str("\n")?;
                for _ in 0..indent {
                    formatter.write_str("  ")?;
                }
                Ok(())
            }
        }
    }

    fn comma(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        match self.indent {
            None => formatter.write_str(", "),
            Some(_) => formatter.write_str(","),
        }
    }
}

fn cvt<T, E>(r: core::result::Result<T, E>) -> Result<T, fmt::Error> {
    match r {
        Ok(v) => Ok(v),
        Err(_) => Err(fmt::Error),
    }
}
use std::fmt::Write;
pub(crate) fn print(
    value: dynamic_value::Reader,
    formatter: &mut Formatter,
    indent: Indent,
) -> Result<(), fmt::Error> {
    match value {
        dynamic_value::Reader::Void => formatter.write_str("{}"),
        dynamic_value::Reader::Bool(b) => formatter.write_fmt(format_args!("{b}")),
        dynamic_value::Reader::Int8(x) => formatter.write_fmt(format_args!("{x}")),
        dynamic_value::Reader::Int16(x) => formatter.write_fmt(format_args!("{x}")),
        dynamic_value::Reader::Int32(x) => formatter.write_fmt(format_args!("{x}")),
        dynamic_value::Reader::Int64(x) => formatter.write_fmt(format_args!("{x}")),
        dynamic_value::Reader::UInt8(x) => formatter.write_fmt(format_args!("{x}")),
        dynamic_value::Reader::UInt16(x) => formatter.write_fmt(format_args!("{x}")),
        dynamic_value::Reader::UInt32(x) => formatter.write_fmt(format_args!("{x}")),
        dynamic_value::Reader::UInt64(x) => formatter.write_fmt(format_args!("{x}")),
        dynamic_value::Reader::Float32(x) => formatter.write_fmt(format_args!("{x}")),
        dynamic_value::Reader::Float64(x) => formatter.write_fmt(format_args!("{x}")),
        dynamic_value::Reader::Enum(e) => match cvt(e.get_enumerant())? {
            Some(enumerant) => {
                formatter.write_str("\"")?;
                formatter.write_str(cvt(cvt(enumerant.get_proto().get_name())?.to_str())?)?;
                formatter.write_str("\"")
            }
            None => formatter.write_fmt(format_args!("{}", e.get_value())),
        },
        dynamic_value::Reader::Text(t) => formatter.write_fmt(format_args!("{t:?}")),
        dynamic_value::Reader::Data(d) => {
            const MAX_LENGTH: usize = 16;  // Define the maximum length of each segment            
            const MAX_UTF8_LENGTH: usize = 64;
            // Attempt to convert byte slice to a UTF-8 string
            let indent2 = indent.next_with_num(5);
            match std::str::from_utf8(d) {
                Ok(text) => {
                    // If the data is valid UTF-8, output as a regular string
                    formatter.write_str("\"")?; // Use double quotes for UTF-8 string output
                    let mut count = 0;
                    for c in text.chars() {
                        if count >= MAX_UTF8_LENGTH {
                            indent2.maybe_newline(formatter)?;  // Break into a new line
                            count = 0;
                        }
                        match c {
                            '\n' => formatter.write_str("\\n")?,
                            '\r' => formatter.write_str("\\r")?,
                            '\t' => formatter.write_str("\\t")?,
                            '"'  => formatter.write_str("\\\"")?,  // Escape double quote
                            '\\' => formatter.write_str("\\\\")?, // Escape backslash
                            _    => formatter.write_char(c)?,
                        }
                        count += c.len_utf8();
                    }
                    formatter.write_str("\"")?; // Close the string with double quotes
                },
                Err(_) => {
                    // If the data is not valid UTF-8, fallback to hexadecimal representation
                    formatter.write_str("b'")?; // Start with "b'" for byte string output
                    for (i, chunk) in d.chunks(MAX_LENGTH).enumerate() {
                        if i > 0 {
                            formatter.write_str("'")?;
                            indent2.comma(formatter)?;
                            indent2.maybe_newline(formatter)?;
                            formatter.write_str(" b'")?; // Start new byte string segment on a new line
                        }
                        for &b in chunk {
                            formatter.write_fmt(format_args!("\\x{:02x}", b))?;
                        }
                    }
                    formatter.write_str("'")?; // Close the final string
                }
            }
        
            Ok(())
        }
        dynamic_value::Reader::List(list) => {
            if list.is_empty() {
                formatter.write_str("[]")
            } else {
                formatter.write_str("[")?;
                let indent2 = indent.next();
                for (idx, value) in list.iter().enumerate() {
                    indent2.maybe_newline(formatter)?;
                    print(cvt(value)?, formatter, indent2)?;
                    if idx + 1 < list.len() as usize {
                        indent2.comma(formatter)?;
                    }
                }
                indent.maybe_newline(formatter)?;
                formatter.write_str("]")
            }
        }
        dynamic_value::Reader::Struct(st) => {
            let schema = st.get_schema();
            let union_fields = cvt(schema.get_union_fields())?;
            let non_union_fields = cvt(schema.get_non_union_fields())?;
            if union_fields.len() + non_union_fields.len() == 0 {
                return formatter.write_str("{}");
            }
            formatter.write_str("{")?;
            let indent2 = indent.next();
            let mut union_field = match cvt(st.which())? {
                None => None,
                Some(field) => {
                    // If it's not the default descriminant, then we always need to print it.
                    if field.get_proto().get_discriminant_value() != 0 || cvt(st.has(field))? {
                        Some(field)
                    } else {
                        None
                    }
                }
            };
            let mut first = true;
            for field in non_union_fields {
                if let Some(ff) = union_field {
                    if ff.get_index() < field.get_index() {
                        // It's time to print the union field.
                        if first {
                            first = false
                        } else {
                            indent2.comma(formatter)?;
                        }
                        indent2.maybe_newline(formatter)?;
                        formatter.write_str("\"")?;
                        formatter.write_str(cvt(cvt(ff.get_proto().get_name())?.to_str())?)?;
                        
                        formatter.write_str("\"")?;
                        formatter.write_str(" : ")?;
                        print(cvt(st.get(ff))?, formatter, indent2)?;
                        union_field = None;
                    }
                }
                if cvt(st.has(field))? {
                    if first {
                        first = false
                    } else {
                        indent2.comma(formatter)?;
                    }
                    indent2.maybe_newline(formatter)?;
                    formatter.write_str("\"")?;
                    formatter.write_str(cvt(cvt(field.get_proto().get_name())?.to_str())?)?;
                    formatter.write_str("\"")?;
                    formatter.write_str(" : ")?;
                    print(cvt(st.get(field))?, formatter, indent2)?;
                    
                }
            }
            if let Some(ff) = union_field {
                // Union field comes last.
                if !first {
                    indent2.comma(formatter)?;
                }
                indent2.maybe_newline(formatter)?;
                formatter.write_str("\"")?;
                formatter.write_str(cvt(cvt(ff.get_proto().get_name())?.to_str())?)?;
                formatter.write_str("\"")?;
                formatter.write_str(" : ")?;
                print(cvt(st.get(ff))?, formatter, indent2)?;
            }
            indent.maybe_newline(formatter)?;
            formatter.write_str("}")
        }
        dynamic_value::Reader::AnyPointer(_) => formatter.write_str("<opaque pointer>"),
        dynamic_value::Reader::Capability(_) => formatter.write_str("<external capability>"),
    }
}

impl<'a> fmt::Debug for dynamic_value::Reader<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        let indent = if f.alternate() {
            Indent::enabled()
        } else {
            Indent::no_indent()
        };
        print(*self, f, indent)
    }
}
