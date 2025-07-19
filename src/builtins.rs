use crate::value::{Value, WqError, WqResult};
use std::collections::HashMap;
use rand::Rng;

/// builtin functions
pub struct Builtins {
    functions: HashMap<String, fn(&[Value]) -> WqResult<Value>>,
}

impl Builtins {
    pub fn new() -> Self {
        let mut builtins = Builtins {
            functions: HashMap::new(),
        };
        builtins.register_functions();
        builtins
    }

    fn register_functions(&mut self) {
        // Arithmetic functions
        self.functions.insert("abs".to_string(), abs);
        self.functions.insert("neg".to_string(), neg);
        self.functions.insert("signum".to_string(), signum);
        self.functions.insert("sqrt".to_string(), sqrt);
        self.functions.insert("exp".to_string(), exp);
        self.functions.insert("log".to_string(), log);
        self.functions.insert("floor".to_string(), floor);
        self.functions.insert("ceiling".to_string(), ceiling);
        self.functions.insert("rand".to_string(), rand);

        // List functions
        self.functions.insert("count".to_string(), count);
        self.functions.insert("first".to_string(), first);
        self.functions.insert("last".to_string(), last);
        self.functions.insert("reverse".to_string(), reverse);
        self.functions.insert("sum".to_string(), sum);
        self.functions.insert("max".to_string(), max);
        self.functions.insert("min".to_string(), min);
        self.functions.insert("avg".to_string(), avg);

        // Generation functions
        self.functions.insert("til".to_string(), til);
        self.functions.insert("range".to_string(), range);

        // Type functions
        self.functions.insert("type".to_string(), type_of);
        self.functions.insert("string".to_string(), to_string);

        // List manipulation
        self.functions.insert("take".to_string(), take);
        self.functions.insert("drop".to_string(), drop);
        self.functions.insert("where".to_string(), where_func);
        self.functions.insert("distinct".to_string(), distinct);
        self.functions.insert("sort".to_string(), sort);

        // Logical functions
        self.functions.insert("and".to_string(), and);
        self.functions.insert("or".to_string(), or);
        self.functions.insert("not".to_string(), not);
    }

    pub fn call(&self, name: &str, args: &[Value]) -> WqResult<Value> {
        if let Some(func) = self.functions.get(name) {
            func(args)
        } else {
            Err(WqError::DomainError(format!("Unknown function: {}", name)))
        }
    }

    pub fn has_function(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }

    pub fn list_functions(&self) -> Vec<String> {
        self.functions.keys().cloned().collect()
    }
}

impl Default for Builtins {
    fn default() -> Self {
        Self::new()
    }
}

// Arithmetic functions
fn abs(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError("abs expects 1 argument".to_string()));
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Int(n.abs())),
        Value::Float(f) => Ok(Value::Float(f.abs())),
        Value::List(items) => {
            let result: WqResult<Vec<Value>> = items.iter().map(|v| abs(&[v.clone()])).collect();
            Ok(Value::List(result?))
        }
        _ => Err(WqError::TypeError("abs only works on numbers".to_string())),
    }
}

fn neg(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError("neg expects 1 argument".to_string()));
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Int(-n)),
        Value::Float(f) => Ok(Value::Float(-f)),
        Value::List(items) => {
            let result: WqResult<Vec<Value>> = items.iter().map(|v| neg(&[v.clone()])).collect();
            Ok(Value::List(result?))
        }
        _ => Err(WqError::TypeError("neg only works on numbers".to_string())),
    }
}

fn signum(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError(
            "signum expects 1 argument".to_string(),
        ));
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Int(if *n > 0 {
            1
        } else if *n < 0 {
            -1
        } else {
            0
        })),
        Value::Float(f) => Ok(Value::Float(if *f > 0.0 {
            1.0
        } else if *f < 0.0 {
            -1.0
        } else {
            0.0
        })),
        Value::List(items) => {
            let result: WqResult<Vec<Value>> = items.iter().map(|v| signum(&[v.clone()])).collect();
            Ok(Value::List(result?))
        }
        _ => Err(WqError::TypeError(
            "signum only works on numbers".to_string(),
        )),
    }
}

fn sqrt(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError("sqrt expects 1 argument".to_string()));
    }

    match &args[0] {
        Value::Int(n) => {
            if *n < 0 {
                Err(WqError::DomainError("sqrt of negative number".to_string()))
            } else {
                Ok(Value::Float((*n as f64).sqrt()))
            }
        }
        Value::Float(f) => {
            if *f < 0.0 {
                Err(WqError::DomainError("sqrt of negative number".to_string()))
            } else {
                Ok(Value::Float(f.sqrt()))
            }
        }
        Value::List(items) => {
            let result: WqResult<Vec<Value>> = items.iter().map(|v| sqrt(&[v.clone()])).collect();
            Ok(Value::List(result?))
        }
        _ => Err(WqError::TypeError("sqrt only works on numbers".to_string())),
    }
}

fn exp(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError("exp expects 1 argument".to_string()));
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Float((*n as f64).exp())),
        Value::Float(f) => Ok(Value::Float(f.exp())),
        Value::List(items) => {
            let result: WqResult<Vec<Value>> = items.iter().map(|v| exp(&[v.clone()])).collect();
            Ok(Value::List(result?))
        }
        _ => Err(WqError::TypeError("exp only works on numbers".to_string())),
    }
}

fn log(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError("log expects 1 argument".to_string()));
    }

    match &args[0] {
        Value::Int(n) => {
            if *n <= 0 {
                Err(WqError::DomainError(
                    "log of non-positive number".to_string(),
                ))
            } else {
                Ok(Value::Float((*n as f64).ln()))
            }
        }
        Value::Float(f) => {
            if *f <= 0.0 {
                Err(WqError::DomainError(
                    "log of non-positive number".to_string(),
                ))
            } else {
                Ok(Value::Float(f.ln()))
            }
        }
        Value::List(items) => {
            let result: WqResult<Vec<Value>> = items.iter().map(|v| log(&[v.clone()])).collect();
            Ok(Value::List(result?))
        }
        _ => Err(WqError::TypeError("log only works on numbers".to_string())),
    }
}

fn floor(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError("floor expects 1 argument".to_string()));
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(f) => Ok(Value::Int(f.floor() as i64)),
        Value::List(items) => {
            let result: WqResult<Vec<Value>> = items.iter().map(|v| floor(&[v.clone()])).collect();
            Ok(Value::List(result?))
        }
        _ => Err(WqError::TypeError(
            "floor only works on numbers".to_string(),
        )),
    }
}

fn ceiling(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError(
            "ceiling expects 1 argument".to_string(),
        ));
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(f) => Ok(Value::Int(f.ceil() as i64)),
        Value::List(items) => {
            let result: WqResult<Vec<Value>> =
                items.iter().map(|v| ceiling(&[v.clone()])).collect();
            Ok(Value::List(result?))
        }
        _ => Err(WqError::TypeError(
            "ceiling only works on numbers".to_string(),
        )),
    }
}

// List functions
fn count(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError("count expects 1 argument".to_string()));
    }
    Ok(Value::Int(args[0].len() as i64))
}

fn first(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError("first expects 1 argument".to_string()));
    }
    match &args[0] {
        Value::List(items) => {
            if items.is_empty() {
                Ok(Value::Null)
            } else {
                Ok(items[0].clone())
            }
        }
        _ => Err(WqError::TypeError("first only works on lists".to_string())),
    }
}

fn last(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError("last expects 1 argument".to_string()));
    }
    match &args[0] {
        Value::List(items) => {
            if items.is_empty() {
                Ok(Value::Null)
            } else {
                Ok(items[items.len() - 1].clone())
            }
        }
        _ => Err(WqError::TypeError("last only works on lists".to_string())),
    }
}

fn reverse(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError(
            "reverse expects 1 argument".to_string(),
        ));
    }
    match &args[0] {
        Value::List(items) => {
            let mut reversed = items.clone();
            reversed.reverse();
            Ok(Value::List(reversed))
        }
        _ => Err(WqError::TypeError(
            "reverse only works on lists".to_string(),
        )),
    }
}

fn sum(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError("sum expects 1 argument".to_string()));
    }
    match &args[0] {
        Value::List(items) => {
            if items.is_empty() {
                return Ok(Value::Int(0));
            }
            let mut result = items[0].clone();
            for item in items.iter().skip(1) {
                result = result
                    .add(item)
                    .ok_or_else(|| WqError::TypeError("Cannot sum these types".to_string()))?;
            }
            Ok(result)
        }
        _ => Ok(args[0].clone()),
    }
}

fn max(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError("max expects 1 argument".to_string()));
    }
    match &args[0] {
        Value::List(items) => {
            if items.is_empty() {
                return Ok(Value::Null);
            }
            let mut result = &items[0];
            for item in items.iter().skip(1) {
                match (result, item) {
                    (Value::Int(a), Value::Int(b)) => {
                        if b > a {
                            result = item;
                        }
                    }
                    (Value::Float(a), Value::Float(b)) => {
                        if b > a {
                            result = item;
                        }
                    }
                    (Value::Int(a), Value::Float(b)) => {
                        if *b > *a as f64 {
                            result = item;
                        }
                    }
                    (Value::Float(a), Value::Int(b)) => {
                        if *b as f64 > *a {
                            result = item;
                        }
                    }
                    _ => return Err(WqError::TypeError("Cannot compare these types".to_string())),
                }
            }
            Ok(result.clone())
        }
        _ => Ok(args[0].clone()),
    }
}

fn min(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError("min expects 1 argument".to_string()));
    }
    match &args[0] {
        Value::List(items) => {
            if items.is_empty() {
                return Ok(Value::Null);
            }
            let mut result = &items[0];
            for item in items.iter().skip(1) {
                match (result, item) {
                    (Value::Int(a), Value::Int(b)) => {
                        if b < a {
                            result = item;
                        }
                    }
                    (Value::Float(a), Value::Float(b)) => {
                        if b < a {
                            result = item;
                        }
                    }
                    (Value::Int(a), Value::Float(b)) => {
                        if *b < *a as f64 {
                            result = item;
                        }
                    }
                    (Value::Float(a), Value::Int(b)) => {
                        if (*b as f64) < *a {
                            result = item;
                        }
                    }
                    _ => return Err(WqError::TypeError("Cannot compare these types".to_string())),
                }
            }
            Ok(result.clone())
        }
        _ => Ok(args[0].clone()),
    }
}

fn avg(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError("avg expects 1 argument".to_string()));
    }
    match &args[0] {
        Value::List(items) => {
            if items.is_empty() {
                return Ok(Value::Null);
            }
            let sum_result = sum(args)?;
            let count = items.len() as f64;
            match sum_result {
                Value::Int(n) => Ok(Value::Float(n as f64 / count)),
                Value::Float(f) => Ok(Value::Float(f / count)),
                _ => Err(WqError::TypeError("Cannot compute average".to_string())),
            }
        }
        _ => Ok(args[0].clone()),
    }
}

// Generation functions
fn til(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError("til expects 1 argument".to_string()));
    }
    match &args[0] {
        Value::Int(n) => {
            if *n < 0 {
                return Err(WqError::DomainError(
                    "til expects non-negative integer".to_string(),
                ));
            }
            let items: Vec<Value> = (0..*n).map(Value::Int).collect();
            Ok(Value::List(items))
        }
        _ => Err(WqError::TypeError("til only works on integers".to_string())),
    }
}

fn range(args: &[Value]) -> WqResult<Value> {
    if args.len() != 2 {
        return Err(WqError::DomainError(
            "range expects 2 arguments".to_string(),
        ));
    }
    match (&args[0], &args[1]) {
        (Value::Int(start), Value::Int(end)) => {
            let items: Vec<Value> = (*start..*end).map(Value::Int).collect();
            Ok(Value::List(items))
        }
        _ => Err(WqError::TypeError(
            "range only works on integers".to_string(),
        )),
    }
}

// helper function for rand
fn to_f64(v: &Value) -> Option<f64> {
    match v {
        Value::Int(n)   => Some(*n as f64),
        Value::Float(f) => Some(*f),
        _               => None,
    }
}

pub fn rand(args: &[Value]) -> WqResult<Value> {
    let mut rng = rand::thread_rng();

    let args: &[Value] = match args {
        [Value::List(inner)] => inner,
        other                => other,
    };

    match args {
        [Value::Int(n)] if *n > 0 => {
            Ok(Value::Int(rng.gen_range(0..*n)))
        }
        [Value::Float(f)] if *f > 0.0 => {
            Ok(Value::Float(rng.gen_range(0.0..*f)))
        }
        [v] => Err(WqError::DomainError(format!(
            "expected positive int or float, got {}",
            v.type_name()
        ))),

        [Value::Int(a), Value::Int(b)] if a < b => {
            Ok(Value::Int(rng.gen_range(*a..*b)))
        }
        [a, b] => {
            let af = to_f64(a).ok_or_else(|| WqError::TypeError(
                format!("expected numbers, got {}", a.type_name())
            ))?;
            let bf = to_f64(b).ok_or_else(|| WqError::TypeError(
                format!("expected numbers, got {}", b.type_name())
            ))?;
            if af < bf {
                Ok(Value::Float(rng.gen_range(af..bf)))
            } else {
                Err(WqError::DomainError(format!(
                    "require a < b, got {} >= {}", af, bf
                )))
            }
        }

        _ => Err(WqError::DomainError(
            "rand expects 1 or 2 arguments".into(),
        )),
    }
}


// Type functions
fn type_of(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError("type expects 1 argument".to_string()));
    }
    Ok(Value::Symbol(args[0].type_name().to_string()))
}

fn to_string(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError(
            "string expects 1 argument".to_string(),
        ));
    }
    Ok(Value::Symbol(args[0].to_string()))
}

// List manipulation
fn take(args: &[Value]) -> WqResult<Value> {
    if args.len() != 2 {
        return Err(WqError::DomainError("take expects 2 arguments".to_string()));
    }
    match (&args[0], &args[1]) {
        (Value::Int(n), Value::List(items)) => {
            let n = *n as usize;
            if n > items.len() {
                Ok(Value::List(items.clone()))
            } else {
                Ok(Value::List(items[..n].to_vec()))
            }
        }
        _ => Err(WqError::TypeError(
            "take expects integer and list".to_string(),
        )),
    }
}

fn drop(args: &[Value]) -> WqResult<Value> {
    if args.len() != 2 {
        return Err(WqError::DomainError("drop expects 2 arguments".to_string()));
    }
    match (&args[0], &args[1]) {
        (Value::Int(n), Value::List(items)) => {
            let n = *n as usize;
            if n >= items.len() {
                Ok(Value::List(Vec::new()))
            } else {
                Ok(Value::List(items[n..].to_vec()))
            }
        }
        _ => Err(WqError::TypeError(
            "drop expects integer and list".to_string(),
        )),
    }
}

fn where_func(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError("where expects 1 argument".to_string()));
    }
    match &args[0] {
        Value::List(items) => {
            let mut indices = Vec::new();
            for (i, item) in items.iter().enumerate() {
                match item {
                    Value::Int(n) => {
                        if *n != 0 {
                            indices.push(Value::Int(i as i64));
                        }
                    }
                    _ => {
                        return Err(WqError::TypeError(
                            "where only works on integer lists".to_string(),
                        ));
                    }
                }
            }
            Ok(Value::List(indices))
        }
        _ => Err(WqError::TypeError("where only works on lists".to_string())),
    }
}

fn distinct(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError(
            "distinct expects 1 argument".to_string(),
        ));
    }
    match &args[0] {
        Value::List(items) => {
            let mut seen = Vec::new();
            for item in items {
                if !seen.contains(item) {
                    seen.push(item.clone());
                }
            }
            Ok(Value::List(seen))
        }
        _ => Err(WqError::TypeError(
            "distinct only works on lists".to_string(),
        )),
    }
}

fn sort(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError("sort expects 1 argument".to_string()));
    }
    match &args[0] {
        Value::List(items) => {
            let mut sorted = items.clone();
            sorted.sort_by(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => x.cmp(y),
                (Value::Float(x), Value::Float(y)) => {
                    x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal)
                }
                (Value::Int(x), Value::Float(y)) => (*x as f64)
                    .partial_cmp(y)
                    .unwrap_or(std::cmp::Ordering::Equal),
                (Value::Float(x), Value::Int(y)) => x
                    .partial_cmp(&(*y as f64))
                    .unwrap_or(std::cmp::Ordering::Equal),
                _ => std::cmp::Ordering::Equal,
            });
            Ok(Value::List(sorted))
        }
        _ => Err(WqError::TypeError("sort only works on lists".to_string())),
    }
}

// Logical functions
fn and(args: &[Value]) -> WqResult<Value> {
    if args.len() != 2 {
        return Err(WqError::DomainError("and expects 2 arguments".to_string()));
    }
    match (&args[0], &args[1]) {
        (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(if *a != false && *b != false {
            true
        } else {
            false
        })),
        _ => Err(WqError::TypeError("and only works on booleans".to_string())),
    }
}

fn or(args: &[Value]) -> WqResult<Value> {
    if args.len() != 2 {
        return Err(WqError::DomainError("or expects 2 arguments".to_string()));
    }
    match (&args[0], &args[1]) {
        (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(if *a != false || *b != false {
            true
        } else {
            false
        })),
        _ => Err(WqError::TypeError("or only works on booleans".to_string())),
    }
}

fn not(args: &[Value]) -> WqResult<Value> {
    if args.len() != 1 {
        return Err(WqError::DomainError("not expects 1 argument".to_string()));
    }
    match &args[0] {
        Value::Bool(n) => Ok(Value::Bool(if *n == false { true } else { false })),
        _ => Err(WqError::TypeError("not only works on booleans".to_string())),
    }
}
