use heck::*;
use std::env;
use std::path::{Path, PathBuf};
use std::process::Command;

macro_rules! codegen_test {
    ($name:ident $test:tt) => {
        #[test]
        fn $name() {
            test_helpers::run_world_codegen_test(
                "guest-csharp",
                $test.as_ref(),
                |world, files| {
                    wit_bindgen_gen_guest_csharp::Opts::default()
                        .build()
                        .generate(world, files)
                },
                verify,
            )
        }
    };
}

test_helpers::codegen_tests!("*.wit");

fn verify(dir: &Path, name: &str) {
    // For C#, we're only generating some different C code + additional C# files
    // This function should be similar to the C one
    // Look to dotnet-wasi-sdk build tasks to make sure
    todo!();
    let path = PathBuf::from(env::var_os("WASI_SDK_PATH").unwrap());
    let mut cmd = Command::new(path.join("bin/clang"));
    cmd.arg("--sysroot").arg(path.join("share/wasi-sysroot"));
    cmd.arg(dir.join(format!("{}.c", name.to_snake_case())));
    cmd.arg("-I").arg(dir);
    cmd.arg("-Wall")
        .arg("-Wextra")
        .arg("-Werror")
        .arg("-Wno-unused-parameter");
    cmd.arg("-c");
    cmd.arg("-o").arg(dir.join("obj.o"));

    test_helpers::run_command(&mut cmd);
}
