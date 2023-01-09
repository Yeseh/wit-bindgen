use heck::{ToLowerCamelCase, ToShoutySnakeCase, ToSnakeCase, ToUpperCamelCase};
use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
    iter, mem,
    ops::Deref,
};
use wit_bindgen_core::{
    uwrite, uwriteln,
    wit_parser::{
        abi::{AbiVariant, Bindgen, Bitcast, Instruction, LiftLower, WasmType},
        Case, Docs, Enum, Flags, FlagsRepr, Function, FunctionKind, Int, Interface, Record,
        Result_, SizeAlign, Tuple, Type, TypeDefKind, TypeId, Union, Variant, World,
    },
    Files, InterfaceGenerator as _, Ns, WorldGenerator,
};

const USINGS: &str = "\
using Wasmtime;
";

#[derive(Default, Debug, Clone)]
#[cfg_attr(feature = "clap", derive(clap::Args))]
pub struct Opts {
    //
}

pub struct WasmtimeCsharp {
    opts: Opts,
    name: String,
    return_area_size: usize,
    return_area_align: usize,
    tuple_counts: HashSet<usize>,
    needs_cleanup: bool,
    needs_result: bool,
    classes: HashMap<String, String>,
}

impl Opts {}
