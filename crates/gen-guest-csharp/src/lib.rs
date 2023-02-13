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
        Case, Docs, Enum, Flags, FlagsRepr, Function, FunctionKind, Int, Interface, InterfaceId,
        Record, Resolve, Result_, SizeAlign, Tuple, Type, TypeDef, TypeDefKind, TypeId, TypeOwner,
        Union, Variant, World, WorldId,
    },
    Files, InterfaceGenerator as _, Ns, WorldGenerator,
};

#[derive(Default, Debug, Clone)]
#[cfg_attr(feature = "clap", derive(clap::Args))]
pub struct Opts {
    generate_stub: bool,
}

struct InterfaceFragment {
    src: String,
    stub: String,
}

#[derive(Default)]
pub struct CSharp {
    opts: Opts,
    name: String,
    return_area_size: usize,
    return_area_align: usize,
    tuple_counts: HashSet<usize>,
    needs_cleanup: bool,
    needs_result: bool,
    world_fragments: Vec<InterfaceFragment>,
    sizes: SizeAlign,
    interface_fragments: HashMap<String, Vec<InterfaceFragment>>,
    interface_names: HashMap<InterfaceId, String>,
}

impl Opts {
    pub fn build(&self) -> Box<dyn WorldGenerator> {
        Box::new(CSharp {
            opts: self.clone(),
            ..CSharp::default()
        })
    }
}

impl CSharp {
    fn qualifier(&self) -> String {
        let world = self.name.to_upper_camel_case();
        format!("{world}World.")
    }

    fn interface<'a>(&'a mut self, resolve: &'a Resolve, name: &'a str) -> InterfaceGenerator<'a> {
        InterfaceGenerator {
            src: String::new(),
            stub: String::new(),
            gen: self,
            resolve,
            name,
        }
    }
}

impl WorldGenerator for CSharp {
    fn preprocess(&mut self, resolve: &Resolve, world: WorldId) {
        let name = &resolve.worlds[world].name;
        self.name = name.to_string();
        self.sizes.fill(resolve);
    }

    fn import_interface(
        &mut self,
        resolve: &Resolve,
        name: &str,
        iface: InterfaceId,
        files: &mut Files,
    ) {
        self.interface_names.insert(iface, name.to_owned());
        let mut gen = self.interface(resolve, name);
        gen.types(iface);

        for (_, func) in resolve.interfaces[iface].functions.iter() {
            gen.import(name, func)
        }

        gen.add_interface_fragment();
    }

    fn export_interface(
        &mut self,
        resolve: &wit_bindgen_core::wit_parser::Resolve,
        name: &str,
        iface: wit_bindgen_core::wit_parser::InterfaceId,
        files: &mut Files,
    ) {
        self.interface_names.insert(iface, name.to_owned());
        let mut gen = self.interface(resolve, name);
        gen.types(iface);

        for (_, func) in resolve.interfaces[iface].functions.iter() {
            gen.export(func, Some(name));
        }

        gen.add_interface_fragment();
    }

    fn import_funcs(
        &mut self,
        resolve: &wit_bindgen_core::wit_parser::Resolve,
        world: wit_bindgen_core::wit_parser::WorldId,
        funcs: &[(&str, &Function)],
        files: &mut Files,
    ) {
        let name = &format!("{}-world", resolve.worlds[world].name);
        let mut gen = self.interface(resolve, name);

        for (_, func) in funcs {
            gen.import(name, func);
        }

        gen.add_world_fragment();
    }

    fn export_funcs(
        &mut self,
        resolve: &wit_bindgen_core::wit_parser::Resolve,
        world: wit_bindgen_core::wit_parser::WorldId,
        funcs: &[(&str, &Function)],
        files: &mut Files,
    ) {
        let name = &format!("{}-world", resolve.worlds[world].name);
        let mut gen = self.interface(resolve, name);

        for (_, func) in funcs {
            gen.export(func, None);
        }

        gen.add_world_fragment();
    }

    fn export_types(
        &mut self,
        resolve: &wit_bindgen_core::wit_parser::Resolve,
        world: wit_bindgen_core::wit_parser::WorldId,
        types: &[(&str, TypeId)],
        files: &mut Files,
    ) {
        let name = &format!("{}-world", resolve.worlds[world].name);
        let mut gen = self.interface(resolve, name);

        for (ty_name, ty) in types {
            gen.define_type(ty_name, *ty);
        }

        gen.add_world_fragment();
    }

    fn finish(&mut self, resolve: &Resolve, id: WorldId, files: &mut Files) {
        let world = &resolve.worlds[id];
        let namespace = format!("wit_{}", world.name.to_snake_case());
        let name = world.name.to_upper_camel_case();

        let mut src = String::new();

        uwrite!(
            src,
            "namespace {namespace};

            public sealed class {name}World 
            {{
                private {name}World() {{}}
            }}
            "
        );

        if self.needs_result {
            // TODO: Write result class
        }

        if self.needs_cleanup {
            // TODO: Write cleanup class
        }

        // TODO: handle return area in C
        if self.return_area_align > 0 {
            // let size = self.return_area_size;
            // let align = self.return_area_align;

            // uwriteln!(src, "public static IntPtr RETURN_AREA = GCHandle.alloc() ")
        }

        src.push_str("}\n");
        files.push(&format!("{name}World.cs"), indent(&src).as_bytes());

        for (name, fragments) in &self.interface_fragments {
            let body = fragments
                .iter()
                .map(|f| f.src.deref())
                .collect::<Vec<_>>()
                .join("/n");

            let body = format!(
                "namespace {namespace}

                public sealed class {name} {{
                    private {name}() {{}}
                    {body}
                }}
                "
            );

            files.push(&format!("{name}.cs"), indent(&body).as_bytes());
        }
    }
}

struct InterfaceGenerator<'a> {
    src: String,
    csrc: String,
    // TODO: From java guest, might need this
    stub: String,
    gen: &'a mut CSharp,
    resolve: &'a Resolve,
    name: &'a str,
}

impl InterfaceGenerator<'_> {
    fn qualifier(&self, when: bool, ty: &TypeDef) -> String {
        if let TypeOwner::Interface(id) = &ty.owner {
            if let Some(name) = self.gen.interface_names.get(id) {
                if name != self.name {
                    return format!("{}.", name.to_upper_camel_case());
                }
            }
        }

        if when {
            let name = self.name.to_upper_camel_case();
            format!("{name}.")
        } else {
            String::new()
        }
    }

    fn add_world_fragment(self) {
        self.gen.world_fragments.push(InterfaceFragment {
            src: self.src,
            stub: self.stub,
        });
    }

    fn add_interface_fragment(self) {
        self.gen
            .interface_fragments
            .entry(self.name.to_upper_camel_case())
            .or_default()
            .push(InterfaceFragment {
                src: self.src,
                stub: self.stub,
            });
    }

    fn import(&mut self, module: &str, func: &Function) {
        if func.kind != FunctionKind::Freestanding {
            todo!("resources")
        }

        let mut bindgen = FunctionBindgen::new(
            self,
            &func.name,
            func.params
                .iter()
                .map(|(name, _)| name.to_csharp_ident())
                .collect(),
        );

        bindgen.gen.resolve.call(
            AbiVariant::GuestImport,
            LiftLower::LowerArgsLiftResults,
            func,
            &mut bindgen,
        );

        let src = bindgen.src;

        let cleanup_list = if bindgen.needs_cleanup_list {
            self.gen.needs_cleanup = true;
            let qualifier = self.gen.qualifier();

            format!("List<{qualifier}Cleanup> cleanupList = new List<{qualifier}Cleanup>();\n",)
        } else {
            String::new()
        };

        let name = &func.name;
        let sig = self.resolve.wasm_signature(AbiVariant::GuestImport, func);
        let result_type = match &sig.results[..] {
            [] => "void",
            [result] => wasm_type(*result),
            _ => unreachable!(),
        };
        let camel_name = &func.name.to_upper_camel_case();

        let params = sig
            .params
            .iter()
            .enumerate()
            .map(|(i, param)| {
                let ty = wasm_type(*param);
                format!("{ty} p{i}")
            })
            .collect::<Vec<_>>()
            .join(", ");

        let sig = self.sig_string(func, false);

        // TODO: This only generates methods, should be added to 'Native' subclass
        uwrite!(
            self.src,
            r#"
                public static extern {result_type} {camel_name}({params});

                {sig} 
                {{
                    {cleanup_list} {src}
                }}
            "#
        )
    }

    fn export(&mut self, func: &Function, interface_name: Option<&str>) {
        let sig = self.resolve.wasm_signature(AbiVariant::GuestExport, func);
        // TODO: Needed?
        // let export_name = func.core_export_name(interface_name);

        let mut bindgen = FunctionBindgen::new(
            self,
            &func.name,
            (0..sig.params.len()).map(|i| format!("p{i}")).collect(),
        );

        bindgen.gen.resolve.call(
            AbiVariant::GuestExport,
            LiftLower::LiftArgsLowerResults,
            func,
            &mut bindgen,
        );

        assert!(!bindgen.needs_cleanup_list);

        let src = bindgen.src;
        let camel_name = func.name.to_upper_camel_case();
        let result_type = match &sig.results[..] {
            [] => "void",
            [result] => wasm_type(*result),
            _ => unreachable!(),
        };

        let params = sig
            .params
            .iter()
            .enumerate()
            .map(|(i, param)| {
                let ty = wasm_type(*param);
                format!("{ty} p{i}")
            })
            .collect::<Vec<_>>()
            .join(", ");

        // TODO: Only generates functions, should be put in a separate class
        // or use some stub attribute [WasiExport]
        uwrite!(
            self.src,
            r#"
            public static {result_type} {camel_name}({params}) 
            {{
                {src}
            }}
            "#
        );

        if self.resolve.guest_export_needs_post_return(func) {
            let params = sig
                .results
                .iter()
                .enumerate()
                .map(|(i, param)| {
                    let ty = wasm_type(*param);
                    format!("{ty} p{i}")
                })
                .collect::<Vec<_>>()
                .join(", ");

            let mut bindgen = FunctionBindgen::new(
                self,
                "INVALID",
                (0..sig.results.len()).map(|i| format!("p{i}")).collect(),
            );

            bindgen.gen.resolve.post_return(func, &mut bindgen);

            let src = bindgen.src;

            uwrite!(
                self.src,
                r#"
                private static extern void wasmExport{camel_name}PostReturn({params}) 
                {{
                    {src}
                }}
                "#
            )
        }

        // TODO: check stub stuff
        if self.gen.opts.generate_stub {}
    }

    fn type_name(&mut self, ty: &Type) -> String {
        self.type_name_with_qualifier(ty, false)
    }

    // TODO: Check this
    fn print_docs(&mut self, docs: &Docs) {
        if let Some(docs) = &docs.contents {
            let lines = docs
                .trim()
                .lines()
                .map(|line| format!("/// {line}"))
                .collect::<Vec<_>>()
                .join("\n");

            uwrite!(self.src, "{lines}")
        }
    }

    fn non_empty_type<'a>(&self, ty: Option<&'a Type>) -> Option<&'a Type> {
        if let Some(ty) = ty {
            let id = match ty {
                Type::Id(id) => *id,
                _ => return Some(ty),
            };
            match &self.resolve.types[id].kind {
                TypeDefKind::Type(t) => self.non_empty_type(Some(t)).map(|_| ty),
                TypeDefKind::Record(r) => (!r.fields.is_empty()).then_some(ty),
                TypeDefKind::Tuple(t) => (!t.types.is_empty()).then_some(ty),
                _ => Some(ty),
            }
        } else {
            None
        }
    }

    fn type_name_with_qualifier(&mut self, ty: &Type, qualifier: bool) -> String {
        match ty {
            Type::Bool => "bool".into(),
            Type::U8 | Type::S8 => "byte".into(),
            Type::U16 | Type::S16 => "short".into(),
            Type::U32 | Type::S32 | Type::Char => "int".into(),
            Type::U64 | Type::S64 => "long".into(),
            Type::Float32 => "float".into(),
            Type::Float64 => "double".into(),
            Type::String => "string".into(),
            Type::Id(id) => {
                let ty = &self.resolve.types[*id];
                match &ty.kind {
                    TypeDefKind::Type(ty) => self.type_name_with_qualifier(ty, qualifier),
                    TypeDefKind::List(ty) => {
                        if is_primitive(ty) {
                            format!("{}[]", self.type_name(ty))
                        } else {
                            format!("List<{}>", self.type_name(ty))
                        }
                    }
                    TypeDefKind::Tuple(tuple) => {
                        // TODO: ripped from java, check c# tuple implementation
                        let count = tuple.types.len();
                        self.gen.tuple_counts.insert(count);

                        let params = if count == 0 {
                            String::new()
                        } else {
                            format!(
                                "<{}>",
                                tuple
                                    .types
                                    .iter()
                                    // TODO: check boxed primitives
                                    .map(|ty| self.type_name(ty))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            )
                        };

                        format!("{}Tuple{count}{params}", self.gen.qualifier())
                    }
                    TypeDefKind::Option(ty) => self.type_name(ty),
                    TypeDefKind::Result(result) => {
                        self.gen.needs_result = true;
                        let mut name = |ty: &Option<Type>| {
                            ty.as_ref().map(|ty| self.type_name(ty)).unwrap_or_else(|| {
                                self.gen.tuple_counts.insert(0);

                                format!("{}Tuple0", self.gen.qualifier())
                            })
                        };
                        let ok = name(&result.ok);
                        let err = name(&result.err);

                        format!("{}Result<{ok}, {err}>", self.gen.qualifier())
                    }
                    _ => {
                        if let Some(name) = &ty.name {
                            format!(
                                "{}{}",
                                self.qualifier(qualifier, ty),
                                name.to_upper_camel_case()
                            )
                        } else {
                            unreachable!()
                        }
                    }
                }
            }
        }
    }

    // TODO: I think these are java specific, C# doesn't generally use boxed primitives
    // fn type_name_boxed(&mut self, ty: &Type, qualifier: bool) -> String {
    //     return self.type_name(ty);
    // match ty {
    //     Type::Bool => "Boolean".into(),
    //     Type::U8 | Type::S8 => "Byte".into(),
    //     Type::U16 | Type::S16 => "Short".into(),
    //     Type::U32 | Type::S32 | Type::Char => "Integer".into(),
    //     Type::U64 | Type::S64 => "Long".into(),
    //     Type::Float32 => "Float".into(),
    //     Type::Float64 => "Double".into(),
    //     Type::Id(id) => {
    //         let def = &self.resolve.types[*id];
    //         match &def.kind {
    //             TypeDefKind::Type(ty) => self.type_name_boxed(ty, qualifier),
    //             _ => self.type_name_with_qualifier(ty, qualifier),
    //         }
    //     }
    //     _ => self.type_name_with_qualifier(ty, qualifier),
    // }
    // }

    fn sig_string(&mut self, func: &Function, qualifier: bool) -> String {
        let name = func.name.to_lower_camel_case();

        let result_type = match func.results.len() {
            0 => "void".into(),
            1 => {
                self.type_name_with_qualifier(func.results.iter_types().next().unwrap(), qualifier)
            }
            count => {
                self.gen.tuple_counts.insert(count);
                format!(
                    "{}Tuple{count}<{}>",
                    self.gen.qualifier(),
                    func.results
                        .iter_types()
                        .map(|ty| self.type_name(ty))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        };

        let params = func
            .params
            .iter()
            .map(|(name, ty)| {
                let ty = self.type_name_with_qualifier(ty, qualifier);
                let name = name.to_lower_camel_case();
                format!("{ty} {name}")
            })
            .collect::<Vec<_>>()
            .join(", ");

        format!("public static {result_type} {name}({params})")
    }
}

impl<'a> wit_bindgen_core::InterfaceGenerator<'a> for InterfaceGenerator<'a> {
    fn resolve(&self) -> &'a Resolve {
        self.resolve
    }

    // TODO: Little bit of a 'decision' here using record instead of class
    fn type_record(&mut self, id: TypeId, name: &str, record: &Record, docs: &Docs) {
        self.print_docs(docs);

        // let parameters = record.fields.iter().map(|field| {
        //     format!(
        //         "{} {}",
        //         self.type_name(&field.ty),
        //         field.name.to_lower_camel_case()
        //     )
        // });

        let fields = record
            .fields
            .iter()
            .map(|field| {
                format!(
                    "{} {}",
                    self.type_name(&field.ty),
                    field.name.to_csharp_ident()
                )
            })
            .collect::<Vec<_>>()
            .join(", ");

        uwrite!(
            self.src,
            "
            public sealed record {name}({fields});
            "
        )
    }

    fn type_flags(&mut self, id: TypeId, name: &str, flags: &Flags, docs: &Docs) {
        self.print_docs(docs);
        let name = name.to_upper_camel_case();
        let ty = match flags.repr() {
            FlagsRepr::U8 => "byte",
            FlagsRepr::U16 => "short",
            FlagsRepr::U32(1) => "int",
            FlagsRepr::U32(2) => "long",
            repr => todo!("flags {repr:?}"),
        };

        let flags = flags
            .flags
            .iter()
            .enumerate()
            .map(|(i, flag)| {
                let flag_name = flag.name.to_shouty_snake_case();
                format!("{flag_name} = (1 << {i});")
            })
            .collect::<Vec<_>>()
            .join("\n");

        uwrite!(
            self.src,
            "
            public enum {name} : {ty}
            {{
                {flags}
            }}
            "
        );
    }

    fn type_tuple(&mut self, id: TypeId, name: &str, flags: &Tuple, docs: &Docs) {
        self.type_name(&Type::Id(id));
    }

    fn type_variant(&mut self, id: TypeId, name: &str, variant: &Variant, docs: &Docs) {
        self.print_docs(docs);

        let name = name.to_upper_camel_case();
        let tag_type = int_type(variant.tag());

        let constructors = variant
            .cases
            .iter()
            .map(|case| {
                let case_name = case.name.to_lower_camel_case();
                let tag = case.name.to_shouty_snake_case();
                let (parameter, argument) = if let Some(ty) = self.non_empty_type(case.ty.as_ref())
                {
                    (
                        format!("{} {case_name}", self.type_name(ty)),
                        case_name.deref(),
                    )
                } else {
                    (String::new(), "null")
                };

                format!(
                    "
                    public static {name} {case_name}({parameter}) 
                    {{
                        return new {name}({tag}, {argument})
                    }}
                    "
                )
            })
            .collect::<Vec<_>>()
            .join("\n");

        // TODO: as getters/setters instead of methods?
        let accessors = variant
            .cases
            .iter()
            .filter_map(|case| {
                self.non_empty_type(case.ty.as_ref()).map(|ty| {
                    let case_name = case.name.to_upper_camel_case();
                    let tag = case.name.to_shouty_snake_case();
                    let ty = self.type_name(ty);
                    format!(
                        r#"public {ty} Get{case_name}() {{
                            if (this.tag = {tag}) 
                            {{
                                return ({ty}) this.value;
                            }}
                            else
                            {{
                                throw new Exception($"Expected {tag}, got " + this.tag);
                            }}
                        }}"#
                    )
                })
            })
            .collect::<Vec<_>>()
            .join("\n");

        let tags = variant
            .cases
            .iter()
            .enumerate()
            .map(|(i, case)| {
                let tag = case.name.to_shouty_snake_case();
                format!("public static {tag_type} {tag} = {i}")
            })
            .collect::<Vec<_>>()
            .join("\n");

        uwrite!(
            self.src,
            "
            public static sealed class {name}
            {{
                public {tag_type} Tag;

                private object value;

                private {name}({tag_type} tag, object value)
                {{
                    this.Tag = tag;
                    this.value = value;
                }}

                {constructors}
                {accessors}
                {tags}
            }}
            "
        )
    }

    fn type_option(&mut self, id: TypeId, name: &str, payload: &Type, docs: &Docs) {
        self.type_name(&Type::Id(id));
    }

    fn type_result(&mut self, id: TypeId, name: &str, result: &Result_, docs: &Docs) {
        self.type_name(&Type::Id(id));
    }

    fn type_union(&mut self, id: TypeId, name: &str, union: &Union, docs: &Docs) {
        self.type_variant(
            id,
            name,
            &Variant {
                cases: union
                    .cases
                    .iter()
                    .enumerate()
                    .map(|(i, case)| Case {
                        docs: case.docs.clone(),
                        name: format!("f{i}"),
                        ty: Some(case.ty),
                    })
                    .collect(),
            },
            docs,
        );
    }

    fn type_enum(&mut self, id: TypeId, name: &str, enum_: &Enum, docs: &Docs) {
        self.print_docs(docs);

        let name = name.to_upper_camel_case();

        let cases = enum_
            .cases
            .iter()
            .map(|case| case.name.to_shouty_snake_case())
            .collect::<Vec<_>>()
            .join(",\n");

        uwrite!(
            self.src,
            "
            public enum {name} 
            {{
                {cases}
            }}
            "
        );
    }

    fn type_alias(&mut self, id: TypeId, name: &str, ty: &Type, docs: &Docs) {
        self.type_name(&Type::Id(id));
    }

    fn type_list(&mut self, id: TypeId, name: &str, ty: &Type, docs: &Docs) {
        self.type_name(&Type::Id(id));
    }

    fn type_builtin(&mut self, id: TypeId, name: &str, ty: &Type, docs: &Docs) {
        unimplemented!();
    }
}

struct Block {
    body: String,
    results: Vec<String>,
    element: String,
    base: String,
}

struct Cleanup {
    address: String,
    size: String,
    align: usize,
}

struct BlockStorage {
    body: String,
    element: String,
    base: String,
    cleanup: Vec<Cleanup>,
}

struct FunctionBindgen<'a, 'b> {
    gen: &'b mut InterfaceGenerator<'a>,
    func_name: &'b str,
    params: Box<[String]>,
    src: String,
    locals: Ns,
    block_storage: Vec<BlockStorage>,
    blocks: Vec<Block>,
    payloads: Vec<String>,
    cleanup: Vec<Cleanup>,
    needs_cleanup_list: bool,
}

impl<'a, 'b> FunctionBindgen<'a, 'b> {
    fn new(
        gen: &'b mut InterfaceGenerator<'a>,
        func_name: &'b str,
        params: Box<[String]>,
    ) -> FunctionBindgen<'a, 'b> {
        Self {
            gen,
            func_name,
            params,
            src: String::new(),
            locals: Ns::default(),
            block_storage: Vec::new(),
            blocks: Vec::new(),
            payloads: Vec::new(),
            cleanup: Vec::new(),
            needs_cleanup_list: false,
        }
    }

    fn lower_variant(
        &mut self,
        cases: &[(&str, Option<Type>)],
        lowered_types: &[WasmType],
        op: &str,
        results: &mut Vec<String>,
    ) {
        todo!()
    }

    fn lift_variant(
        &mut self,
        ty: &Type,
        cases: &[(&str, Option<Type>)],
        op: &str,
        results: &mut Vec<String>,
    ) {
        todo!()
    }
}

impl Bindgen for FunctionBindgen<'_, '_> {
    type Operand = String;

    fn emit(
        &mut self,
        resolve: &Resolve,
        inst: &Instruction<'_>,
        operands: &mut Vec<Self::Operand>,
        results: &mut Vec<Self::Operand>,
    ) {
        match inst {
            Instruction::GetArg { nth } => results.push(self.params[*nth].clone()),
            Instruction::I32Const { val } => results.push(val.to_string()),
            Instruction::ConstZero { tys } => results.extend(tys.iter().map(|ty| {
                match ty {
                    WasmType::I32 => "0",
                    WasmType::I64 => "0L",
                    WasmType::F32 => "0.0f",
                    WasmType::F64 => "0.0d",
                }
                .to_owned()
            })),
            // TODO: from java impl, checked
            Instruction::U8FromI32 => results.push(format!("(byte) ({})", operands[0])),
            Instruction::S8FromI32 => results.push(format!("(byte) ({})", operands[0])),
            Instruction::U16FromI32 => results.push(format!("(short) ({})", operands[0])),
            Instruction::S16FromI32 => results.push(format!("(short) ({})", operands[0])),
            Instruction::I32FromU8 => results.push(format!("((int) ({})) & 0xFF", operands[0])),
            Instruction::I32FromU16 => results.push(format!("((int) ({})) & 0xFFFF", operands[0])),
            Instruction::I32FromS8 | Instruction::I32FromS16 => {
                results.push(format!("(int) ({})", operands[0]))
            }
            Instruction::CharFromI32
            | Instruction::I32FromChar
            | Instruction::U32FromI32
            | Instruction::S32FromI32
            | Instruction::S64FromI64
            | Instruction::U64FromI64
            | Instruction::I32FromU32
            | Instruction::I32FromS32
            | Instruction::I64FromS64
            | Instruction::I64FromU64
            | Instruction::F32FromFloat32
            | Instruction::F64FromFloat64
            | Instruction::Float32FromF32
            | Instruction::Float64FromF64 => results.push(operands[0].clone()),

            Instruction::Bitcasts { casts } => {
                results.extend(casts.iter().zip(operands).map(|(cast, op)| match cast {
                    Bitcast::I32ToF32 => {
                        format!("BitConverter.ToSingle(BitConverter.GetBytes({op}), 0)")
                    }
                    Bitcast::I64ToF32 => {
                        format!("BitConverter.ToSingle(BitConverter.GetBytes({op}), 0)")
                    }
                    Bitcast::F32ToI32 => {
                        format!("BitConverter.ToInt32(BitConverter.GetBytes({op}), 0)")
                    }
                    Bitcast::F32ToI64 => {
                        format!("BitConverter.ToInt64(BitConverter.GetBytes({op}), 0)")
                    }
                    Bitcast::I64ToF64 => format!("BitConverter.Int64BitsToDouble({op})"),
                    Bitcast::F64ToI64 => format!("BitConverter.DoubleToInt64Bits({op})"),
                    Bitcast::I32ToI64 => format!("(long) ({op}))"),
                    Bitcast::I64ToI32 => format!("(int) ({op})"),
                    Bitcast::None => op.to_owned(),
                }))
            }
            // TODO: from java impl, checked
            Instruction::I32FromBool => {
                results.push(format!("({} ? 1 : 0)", operands[0]));
            }
            Instruction::BoolFromI32 => results.push(format!("({} != 0)", operands[0])),

            Instruction::FlagsLower { flags, name, ty } => todo!(),
            Instruction::FlagsLift { flags, name, ty } => todo!(),

            Instruction::RecordLower { record, name, ty } => todo!(),
            Instruction::RecordLift { record, name, ty } => todo!(),

            Instruction::EnumLower { enum_, name, ty } => results.push(format!(
                "Array.IndexOf(Enum.GetValues(typeof({})), {})",
                name.to_upper_camel_case(),
                operands[0]
            )),
            Instruction::EnumLift { enum_, name, ty } => results.push(format!(
                "Enum.GetNames(typeof({})).GetValue({})",
                name.to_upper_camel_case(),
                operands[0]
            )),

            Instruction::I32Load { offset } => todo!(),
            Instruction::I32Load8U { offset } => todo!(),
            Instruction::I32Load8S { offset } => todo!(),
            Instruction::I32Load16U { offset } => todo!(),
            Instruction::I32Load16S { offset } => todo!(),
            Instruction::I64Load { offset } => todo!(),
            Instruction::F32Load { offset } => todo!(),
            Instruction::F64Load { offset } => todo!(),
            Instruction::I32Store { offset } => todo!(),
            Instruction::I32Store8 { offset } => todo!(),
            Instruction::I32Store16 { offset } => todo!(),
            Instruction::I64Store { offset } => todo!(),
            Instruction::F32Store { offset } => todo!(),
            Instruction::F64Store { offset } => todo!(),
            Instruction::ListCanonLower { element, realloc } => todo!(),
            Instruction::StringLower { realloc } => todo!(),
            Instruction::ListLower { element, realloc } => todo!(),
            Instruction::ListCanonLift { element, ty } => todo!(),
            Instruction::StringLift => todo!(),
            Instruction::ListLift { element, ty } => todo!(),
            Instruction::IterElem { element } => todo!(),
            Instruction::IterBasePointer => todo!(),
            Instruction::TupleLower { tuple, ty } => todo!(),
            Instruction::TupleLift { tuple, ty } => todo!(),
            Instruction::VariantPayloadName => todo!(),
            Instruction::VariantLower {
                variant,
                name,
                ty,
                results,
            } => todo!(),
            Instruction::VariantLift { variant, name, ty } => todo!(),
            Instruction::UnionLower {
                union,
                name,
                ty,
                results,
            } => todo!(),
            Instruction::UnionLift { union, name, ty } => todo!(),
            Instruction::OptionLower {
                payload,
                ty,
                results,
            } => todo!(),
            Instruction::OptionLift { payload, ty } => todo!(),
            Instruction::ResultLower {
                result,
                ty,
                results,
            } => todo!(),
            Instruction::ResultLift { result, ty } => todo!(),
            Instruction::CallWasm { name, sig } => todo!(),
            Instruction::CallInterface { func } => todo!(),
            Instruction::Return { amt, func } => todo!(),
            Instruction::Malloc {
                realloc,
                size,
                align,
            } => todo!(),
            Instruction::GuestDeallocate { size, align } => todo!(),
            Instruction::GuestDeallocateString => todo!(),
            Instruction::GuestDeallocateList { element } => todo!(),
            Instruction::GuestDeallocateVariant { blocks } => todo!(),
        }
    }

    fn return_pointer(&mut self, iface: &Interface, size: usize, align: usize) -> Self::Operand {
        todo!()
    }

    fn push_block(&mut self) {
        todo!()
    }

    fn finish_block(&mut self, operand: &mut Vec<Self::Operand>) {
        todo!()
    }

    fn sizes(&self) -> &SizeAlign {
        &self.gen.sizes
    }

    fn is_list_canonical(&self, iface: &Interface, element: &Type) -> bool {
        todo!()
    }
}

fn int_type(int: Int) -> &'static str {
    match int {
        Int::U8 => "byte",
        Int::U16 => "short",
        Int::U32 => "int",
        Int::U64 => "long",
    }
}

fn wasm_type(ty: WasmType) -> &'static str {
    match ty {
        WasmType::I32 => "int",
        WasmType::I64 => "long",
        WasmType::F32 => "float",
        WasmType::F64 => "double",
    }
}

fn flags_repr(flags: &Flags) -> Int {
    match flags.repr() {
        FlagsRepr::U8 => Int::U8,
        FlagsRepr::U16 => Int::U16,
        FlagsRepr::U32(1) => Int::U32,
        FlagsRepr::U32(2) => Int::U64,
        repr => panic!("unimplemented flags {repr:?}"),
    }
}

fn list_element_info(ty: &Type) -> (usize, &'static str) {
    match ty {
        Type::U8 | Type::S8 => (1, "byte"),
        Type::U16 | Type::S16 => (2, "short"),
        Type::U32 | Type::S32 => (4, "int"),
        Type::U64 | Type::S64 => (8, "long"),
        Type::Float32 => (4, "float"),
        Type::Float64 => (8, "double"),
        _ => unreachable!(),
    }
}

fn indent(code: &str) -> String {
    let mut indented = String::with_capacity(code.len());
    let mut indent = 0;
    let mut was_empty = false;
    for line in code.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            if was_empty {
                continue;
            }
            was_empty = true;
        } else {
            was_empty = false;
        }

        if trimmed.starts_with('}') {
            indent -= 1;
        }
        indented.extend(iter::repeat(' ').take(indent * 4));
        indented.push_str(trimmed);
        if trimmed.ends_with('{') {
            indent += 1;
        }
        indented.push('\n');
    }
    indented
}

fn is_primitive(ty: &Type) -> bool {
    matches!(
        ty,
        Type::U8
            | Type::S8
            | Type::U16
            | Type::S16
            | Type::U32
            | Type::S32
            | Type::U64
            | Type::S64
            | Type::Float32
            | Type::Float64
    )
}

trait ToCsharpIdent: ToOwned {
    fn to_csharp_ident(&self) -> Self::Owned;
}

impl ToCsharpIdent for str {
    // Escape C# keywords
    // Source: https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/
    fn to_csharp_ident(&self) -> String {
        match self {
            "abstract" | "as" | "base" | "bool" | "break" | "byte" | "case" | "catch" | "char"
            | "checked" | "class" | "const" | "continue" | "decimal" | "default" | "delegate"
            | "do" | "double" | "else" | "enum" | "event" | "explicit" | "extern" | "false"
            | "finally" | "fixed" | "float" | "for" | "foreach" | "goto" | "if" | "implicit"
            | "in" | "int" | "interface" | "internal" | "is" | "lock" | "long" | "namespace"
            | "new" | "null" | "object" | "operator" | "out" | "override" | "params"
            | "private" | "protected" | "public" | "readonly" | "ref" | "return" | "sbyte"
            | "sealed" | "short" | "sizeof" | "stackalloc" | "static" | "string" | "struct"
            | "switch" | "this" | "throw" | "true" | "try" | "typeof" | "uint" | "ulong"
            | "unchecked" | "unsafe" | "ushort" | "using" | "virtual" | "void" | "volatile"
            | "while" | "add" | "and" | "alias" | "ascending" | "args" | "async" | "await"
            | "by" | "descending" | "dynamic" | "equals" | "file" | "from" | "get" | "global"
            | "group" | "init" | "into" | "join" | "let" | "managed" | "nameof" | "nint"
            | "not" | "notnull" | "nuint" | "on" | "or" | "orderby" | "partial" | "record"
            | "remove" | "required" | "scoped" | "select" | "set" | "unmanaged" | "var"
            | "when" | "where" | "with" | "yield" => format!("@{self}"),
            _ => self.to_lower_camel_case(),
        }
    }
}
