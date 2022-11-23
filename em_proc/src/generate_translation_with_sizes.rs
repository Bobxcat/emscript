use proc_macro::TokenStream;
use quote::{
    quote, ToTokens, TokenStreamExt,
    __private::{Punct, Spacing},
};
use syn::{
    parse::{Parse, ParseBuffer, ParseStream},
    parse_macro_input, Expr, Ident, Type,
};

/// Parses from a method declaration re-statement of the form `method_name(arg1: type1, .., argn: typen) -> type_ret`
#[derive(Debug)]
pub(crate) struct MethodDec {
    pub(crate) name: Ident,
    pub(crate) params: Vec<VarDec>,
    pub(crate) param_idents: Vec<VarIdent>,
    #[allow(unused)]
    pub(crate) param_types: Vec<Type>,
    /// The sizes of each parameter
    pub(crate) param_chunks: Vec<usize>,
    pub(crate) ret: Option<Type>,
    /// The size of `ret`. Will be 0 if `ret` is `None`
    pub(crate) ret_chunks: usize,
}

#[derive(Clone, Debug)]
pub(crate) struct VarIdent(String);

impl ToTokens for VarIdent {
    fn to_tokens(&self, tokens: &mut quote::__private::TokenStream) {
        let e: syn::Expr = syn::parse_str(&self.0).expect("Unable to parse");
        e.to_tokens(tokens);
    }
}

#[derive(Debug, Clone)]
pub(crate) struct VarDec(VarIdent, Type);

impl VarDec {
    #[allow(unused)]
    pub fn name(&self) -> &str {
        &self.0 .0
    }
    pub fn t(&self) -> &Type {
        &self.1
    }
}

impl ToTokens for VarDec {
    fn to_tokens(&self, tokens: &mut quote::__private::TokenStream) {
        self.0.to_tokens(tokens);

        tokens.append(Punct::new(':', Spacing::Alone));

        self.1.to_tokens(tokens);
    }
}

impl Parse for MethodDec {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        //Parse the `fn foo(A, B, ..., N) -> Ret`
        input.parse::<Token![fn]>()?;
        let name: Ident = input.parse()?;

        //Keeps track of the current var name
        let mut var_name: usize = 0;

        let params = {
            let mut params = Vec::new();

            let arg_types: ParseBuffer;
            parenthesized!(arg_types in input);

            while !arg_types.is_empty() {
                let arg_type: Type = arg_types.parse()?;
                params.push(VarDec(VarIdent(format!("_{var_name}")), arg_type));
                var_name += 1;

                if !arg_types.is_empty() {
                    let _comma: Token![,] = arg_types.parse()?;
                }
            }

            params
        };

        //Get the return type
        let ret = if let Ok(_) = input.parse::<Token![->]>() {
            let a = input.parse()?;
            Some(a)
        } else {
            None
        };

        input.parse::<Token![;]>()?;

        let param_idents = params.iter().map(|dec| dec.0.clone()).collect();
        let param_types = params.iter().map(|dec| dec.1.clone()).collect();

        //Parse the `{size_a, size_b, ..., size_n} -> size_ret`

        //Get param sizes
        let param_chunks = {
            let mut param_chunks = Vec::new();

            let arg_sizes: ParseBuffer;
            parenthesized!(arg_sizes in input);

            // println!("arg_sizes: {:#?}", arg_sizes);

            while !arg_sizes.is_empty() {
                let arg_s: Expr = arg_sizes.parse()?;
                param_chunks.push({
                    let expr_string = arg_s.to_token_stream().to_string();
                    expr_string.parse().expect(&format!(
                        "Could not parse a parameter type's size as a `usize`: `{expr_string}`"
                    ))
                });
                var_name += 1;

                if !arg_sizes.is_empty() {
                    let _comma: Token![,] = arg_sizes.parse()?;
                }
            }

            param_chunks
        };

        //Get the return size
        let ret_chunks = if let Ok(_) = input.parse::<Token![->]>() {
            let expr_string = input.parse::<Expr>()?.to_token_stream().to_string();
            expr_string.parse().expect(&format!(
                "Could not parse `ret_size` as a `usize`: `{expr_string}`"
            ))
        } else {
            0
        };

        // let ret = match fn_dec.sig.output {
        //     syn::ReturnType::Default => None,
        //     syn::ReturnType::Type(_, t) => Some((*t).clone()),
        // };

        let m = MethodDec {
            name,
            params,
            param_idents,
            param_types,
            param_chunks,
            ret,
            ret_chunks,
        };

        // println!("MethodDec parsed:\n{:#?}", m);

        Ok(m)
    }
}

// /// Takes in 2 or more expressions, and applies `assert_eq!()` on the first expression with every other
// macro_rules! assert_eq_multi {
//     ($a:expr, $($b:expr),*) => {
//        $(assert_eq!($a, $b));*
//     };
// }

/// See `lib.rs`
pub fn generate_translation_with_sizes(input: TokenStream) -> TokenStream {
    if cfg!(debug_assertions) {
        println!(
            "===Started `generate_translation_with_sizes`===\nInput: {}",
            input
        )
    }

    // Parse the input tokens into a syntax tree
    let MethodDec {
        name,
        params,
        param_idents,
        param_types: _,
        param_chunks,
        ret,
        ret_chunks,
    } = parse_macro_input!(input as MethodDec);
    //Assert that all the `param..` are the same length
    //Removed temporarily to allow for adding parameters
    // assert_eq_multi!(
    //     params.len(),
    //     param_idents.len(),
    //     param_types.len(),
    //     param_chunks.len()
    // );

    // Contains the translations required for each parameter
    // All entries are of the form `let $foo = some_method_probably_using_pointer_magic($foo_translated1, $foo_translated_2,...);`
    let mut param_translations = Vec::new();
    // Contains the parameters of the translating closure
    let mut translated_params: Vec<VarDec> = Vec::new();

    // Counter for `translated_params` generated names. Counts down instead of up to avoid conflict with
    // `var_name` counter in `impl Parse for MethodDec`
    let mut n = usize::MAX;

    //Closure which generates a new variable that's unique within the translation layer
    let mut new = || {
        let v = VarDec(VarIdent(format!("_{n}")), Type::Verbatim(quote!(i32)));
        n -= 1;
        v
    };

    /// Generates `n` variables, all of type `i32` and with unique names, returning them as a
    /// `Vec<VarDec>`
    macro_rules! generate_chunks {
        ($n:expr) => {{
            let mut v = Vec::with_capacity($n);
            for _ in 0..$n {
                v.push(new());
            }

            v
        }};
    }

    const RET_VALUE_PTR_IDX: usize = 1;
    const RET_VALUE_PTR_IDENT: &'static str = "ret_value_ptr";

    //Create a parameter of the type `&WasmEnv` as the first arg to deal with pointer magic, if one doesn't yet exist.
    //If one does exist, it will be dealt with in the normal parameter translation loop
    {
        let has_env = {
            let mut has_env = false;
            for i in 0..params.len() {
                let p = &params[i];
                if p.1.to_token_stream().to_string() == "& WasmEnv" {
                    has_env = true;
                    continue;
                }
            }

            has_env
        };

        //Create a `&WasmEnv` arg as the first param when one doesn't exist already,
        //and since the translated method doesn't need it as a param, no transformations are needed
        if !has_env {
            let dec = VarDec(VarIdent("env".into()), Type::Verbatim(quote!(&WasmEnv)));
            translated_params.push(dec.clone());

            if cfg!(debug_assertions) {
                println!("Created a `&WasmEnv` since one didn't already exist");
            }
        }
    }

    //If the return_value_ptr needs to be created, set it specially.
    //No translations currently need to be done, but it does neded to be added to `translated_params` as a `WasmPtr<u8>`
    if ret_chunks > 1 {
        let dec = VarDec(
            VarIdent(RET_VALUE_PTR_IDENT.into()),
            Type::Verbatim(quote!(WasmPtr<u8>)),
        );
        translated_params.push(dec.clone());
    }

    //Translate all parameters of the untranslated method
    for i in 0..params.len() {
        let p = &params[i];

        //Ignore any parameters of type `&WasmEnv`, treating them as normal parameters:
        //Note: this comparison is *not* stable (kind of)
        if p.1.to_token_stream().to_string() == "& WasmEnv" {
            let dec = VarDec(VarIdent("env".into()), p.1.clone());
            translated_params.push(dec.clone());

            let lhs = &param_idents[i];
            let rhs = &dec.0;
            param_translations.push(quote!(let #lhs = #rhs;));
            if cfg!(debug_assertions) {
                println!("Dealt with translating a `&WasmEnv`");
            }
            continue;
        }

        //Find the best representation for the parameter in terms of WASM-capable types
        //Allows for multiple variable declarations to get any desired size
        //Must fulfill the following criteria:
        //1- Same size as `p.t()` in total
        //2- Contains *only* number types (I32) or references (I32)
        //3- Contains *only* I32
        let mut translated = generate_chunks!(param_chunks[i]);

        //The translation which will take place to represent all values in `translated` as the single value `p.name()`
        //This step will take into account the type of `p`, namely if it is a reference it will treat it properly (as opposed to a raw transmute)
        let translation = {
            let name = &p.0;
            let translated_len = translated.len();
            let translated_idents = translated.iter().map(|dec| &dec.0).collect::<Vec<_>>();
            match p.t() {
                //For a reference use the method `wasm_ptr_as_ref_with_size_mut`
                //it is "guaranteed" that there will be a `&WasmEnv` called `env` in scope, ideally as a parameter of the generated closure
                //this relies on an implementation of `GetRefFromMem` for the corresponding type
                Type::Reference(_) => {
                    //The offset of the wasm ptr (aka the location of the memory according to WASM's 32 bits)
                    let wasm_ptr = translated_idents[0];
                    quote! {let #name = {
                        unsafe {
                            GetRefFromMem::as_mem_ref_mut(#wasm_ptr as usize, env.memory_ref_unchecked()).unwrap()
                        }
                    };}
                }
                _ => {
                    quote! {let #name = {
                        let arr: [i32; #translated_len] = [#(#translated_idents),*];
                        unsafe {std::mem::transmute(arr)}
                    };}
                }
            }
        };

        translated_params.append(&mut translated);
        param_translations.push(translation);
    }

    //Translate return type
    //Note that a `env: &WasmEnv` is guaranteed to exist
    //The translating variable itself is just named `ret`, since there's only 1 of them
    //[IMPORTANT] A note about return values:
    //-If `ret_chunks == 0`, then just return void
    //-If `ret_chunks == 1`, then the return value is returned as normal
    //-If `ret_chunks > 1`, then a pointer to the location of the return value will be passed as the
    //  first argument, after `env: &WasmEnv`
    //  (so the structure will be `|env: &WasmEnv, ret_value_ptr: WasmPtr<u8>, ...|)`
    let ret_translation = if let Some(ret) = ret {
        //Return by populating a reference
        if ret_chunks > 1 {
            // println!("ret: {:#?}", ret);
            match &ret {
                Type::BareFn(_) => todo!(),
                Type::Group(_) => todo!(),
                Type::ImplTrait(_) => todo!(),
                Type::Infer(_) => todo!(),
                Type::Macro(_) => todo!(),
                Type::Never(_) => todo!(),
                // Type::Path(_) => todo!(),
                Type::TraitObject(_) => todo!(),
                Type::Ptr(_) | Type::Slice(_) | Type::Reference(_) => {
                    panic!("Tried to pass a Rust reference to WASM code")
                }
                _ => {
                    // let chunk_idents = chunks.iter().map(|dec| dec.0.clone()).collect::<Vec<_>>();
                    // let chunk_types = chunks[0].1.clone();

                    let ret_val_ptr_ident = translated_params[RET_VALUE_PTR_IDX].0.clone();
                    //^^`ret_val_ptr`
                    // let ret_val_ptr_type = translated_params[RET_VALUE_PTR_IDX].1.clone(); //`WasmPtr<u8>`
                    let untranslated_ret_type = ret.clone(); //Whatever the unstranslated method returns

                    //Get a raw, mutable pointer from the WasmPtr<u8> called `$RET_VALUE_PTR_TYPE`
                    //Get the return value
                    //Populate the pointer using the return value
                    //Return void
                    quote! {
                        let ret_value_ptr: &mut #untranslated_ret_type = unsafe {
                            wasm_ptr_as_ref_mut((#ret_val_ptr_ident).offset() as usize, env.memory_ref_unchecked())
                        };
                        let mut ret: #untranslated_ret_type = #name(#(#param_idents),*);
                        // ret_value_ptr.clone_from(&ret);
                        *ret_value_ptr = ret;
                    }
                }
            }
        //Return by value, by just transmuting to an i32
        } else if ret_chunks == 1 {
            let chunk = generate_chunks!(ret_chunks)[0].clone();
            let chunk_ident = chunk.0.clone();
            quote! {
                let #chunk = unsafe {std::mem::transmute(#name(#(#param_idents),*))};
                #chunk_ident
            }
        //Zero-sized return value, so just return void
        } else {
            quote! {
                #name(#(#param_idents),*);
            }
        }
    } else {
        quote!(#name(#(#param_idents),*))
    };

    // Build the output, possibly using quasi-quotation
    let expanded = quote! {
        {
            |#(#translated_params),*| {
                #(#param_translations)*
                #ret_translation
            }
        }
    };

    if cfg!(debug_assertions) {
        println!("generate_translation exanded:\n{}", expanded.to_string());
    }

    // Hand the output tokens back to the compiler
    TokenStream::from(expanded)
}
