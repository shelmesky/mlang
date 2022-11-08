use clap::{Arg, Command};
use compiler::compiler::compile_file;
use compiler::compiler::CompileOption;
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
            let compile_option = CompileOption {
                cpu_platform: platform.clone(),
                operating_system: os.clone(),
                source_path: path.clone(),
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
