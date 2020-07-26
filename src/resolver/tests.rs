use super::*;
use crate::test_utils::*;

#[test]
fn test_var_in_init() {
    let input = r#"{var x = x + 1;}"#;
    assert_eq!(
        Err(ResolutionError::VarInInitalizer(Loc::new(0, 9))),
        resolve(input)
    )
}

#[test]
fn test_var_already_in_scope() {
    let input = r#"{var x; var x;}"#;
    assert_eq!(
        Err(ResolutionError::VarAlreadyInScope(
            Loc::new(0, 8),
            String::from("x")
        )),
        resolve(input)
    )
}

#[test]
fn test_duplicate_argument() {
    let input = r#"fun my_fun(param, param) {}"#;
    assert_eq!(
        Err(ResolutionError::DuplicateArgumentName(
            Loc::new(0, 18),
            String::from("param")
        )),
        resolve(input)
    )
}

#[test]
fn test_return_outside_fun() {
    let input = r#"return;"#;
    assert_eq!(
        Err(ResolutionError::ReturnOutsideFun(Loc::new(0, 0))),
        resolve(input)
    )
}

#[test]
fn test_break_outside_loop() {
    let input = r#"break;"#;
    assert_eq!(
        Err(ResolutionError::BreakOutsideLoop(Loc::new(0, 0))),
        resolve(input)
    )
}

#[test]
fn test_break_in_fun() {
    let input = r#"
    while(true) {
        fun local_fun() {
            break;
        }
    }
    "#;
    assert_eq!(
        Err(ResolutionError::BreakOutsideLoop(Loc::new(3, 12))),
        resolve(input)
    )
}

#[test]
fn test_break_in_anon_fun() {
    let input = r#"
    while(true) {
        var local_fun = fun() {
            break;
        };
    }
    "#;
    assert_eq!(
        Err(ResolutionError::BreakOutsideLoop(Loc::new(3, 12))),
        resolve(input)
    )
}

#[test]
fn test_multiple() {
    let input = r#"return; break;"#;
    assert_eq!(
        Err(ResolutionError::Multiple(vec![
            ResolutionError::ReturnOutsideFun(Loc::new(0, 0)),
            ResolutionError::BreakOutsideLoop(Loc::new(0, 8)),
        ])),
        resolve(input)
    )
}
