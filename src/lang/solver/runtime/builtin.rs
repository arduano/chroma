pub enum RtInternalBuiltin {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}

pub fn apply_internal_builtin(
    args: &Vec<String>,
    builtin: &RtInternalBuiltin,
) -> Result<String, String> {
    let left = args[0].clone();
    let right = args[1].clone();

    let result = match builtin {
        RtInternalBuiltin::Add => left + &right,
        RtInternalBuiltin::Subtract => left.replace(&right, ""),
        RtInternalBuiltin::Multiply => left.repeat(right.parse().unwrap()),
        RtInternalBuiltin::Divide => left.split(&right).collect::<Vec<&str>>().join(""),
        RtInternalBuiltin::Modulo => left.split(&right).collect::<Vec<&str>>().join(""),
    };

    Ok(result)
}
