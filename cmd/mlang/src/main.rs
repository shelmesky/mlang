use clap::{Arg, Command};
use compiler::compiler::compile_file;
use compiler::compiler::CompileOption;
use goldentests::{TestConfig, TestResult};
use std::process::exit;

fn main() {
    let cmd = Command::new("mlang")
        .bin_name("mlang")
        .about("mlang programming language")
        .subcommand_required(true)
        .arg_required_else_help(true)
        .author("roy.lieu@gmail.com")
        .subcommand(
            Command::new("build")
                .long_flag("build")
                .about("build mlang package")
                .arg(
                    Arg::new("platform")
                        .long("platform")
                        .num_args(1)
                        .default_value("amd64")
                        .help("cpu platform which binary executable file will compile to"),
                )
                .arg(
                    Arg::new("os")
                        .long("os")
                        .num_args(1)
                        .default_value("linux")
                        .help("operation system which binary executable file will compile to"),
                )
                .arg(
                    Arg::new("path")
                        .long("path")
                        .num_args(1)
                        .default_value("./")
                        .help("path of source code file or source code directory"),
                )
                .arg(
                    Arg::new("emit-llvm-ir")
                        .long("emit-llvm-ir")
                        .num_args(1)
                        .default_value("false")
                        .help("emit llvm ir content to disk file and console"),
                )
                .arg(
                    Arg::new("emit-asm")
                        .long("emit-asm")
                        .num_args(1)
                        .default_value("false")
                        .help("emit machine assembly to disk file and console"),
                )
                .arg(
                    Arg::new("emit-object-file")
                        .long("emit-object-file")
                        .num_args(1)
                        .default_value("true")
                        .help("write object file of a source code file"),
                )
                .arg(
                    Arg::new("run-binary")
                        .long("run-binary")
                        .num_args(1)
                        .default_value("false")
                        .help("run binary file when compile was finished"),
                )
                .arg(
                    Arg::new("delete-binary")
                        .long("delete-binary")
                        .num_args(1)
                        .default_value("false")
                        .help("delete binary file when compile was finished"),
                ),
        )
        .subcommand(
            Command::new("package")
                .long_flag("package")
                .about("manage mlang package")
                .arg(
                    Arg::new("new")
                        .long("new")
                        .num_args(1)
                        .help("create new mlang project package"),
                ),
        )
        .get_matches();

    match cmd.subcommand() {
        Some(("build", build_match)) => {
            let platform = build_match.get_one::<String>("platform").unwrap();
            let os = build_match.get_one::<String>("os").unwrap();
            let path = build_match.get_one::<String>("path").unwrap();
            let emit_llvm_ir = build_match.get_one::<String>("emit-llvm-ir").unwrap();
            let emit_asm = build_match.get_one::<String>("emit-asm").unwrap();
            let emit_object_file = build_match.get_one::<String>("emit-object-file").unwrap();
            let run_binary = build_match.get_one::<String>("run-binary").unwrap();
            let delete_binary = build_match.get_one::<String>("delete-binary").unwrap();
            let compile_option = CompileOption {
                cpu_platform: platform.clone(),
                operating_system: os.clone(),
                source_path: path.clone(),
                emit_llvm_ir: emit_llvm_ir.clone(),
                emit_asm: emit_asm.clone(),
                emit_object_file: emit_object_file.clone(),
                run_binary: run_binary.clone(),
                delete_binary: delete_binary.clone(),
            };

            compile_file(compile_option.clone());
        }
        Some(("package", _package_match)) => {
            println!("create new mlang package");
            exit(1);
        }
        _ => unreachable!(),
    }
}

#[test]
fn goldentests() -> TestResult<()> {
    let config = TestConfig::new("../../target/debug/mlang", "../../examples", "// ")?;
    config.run_tests()
}
