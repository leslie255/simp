dump=false
log_flag=""
release_flag="--release"
sources=()
fuse_lld_flag=""
run=true

simp_to_o() {
    local simp_file="$1"
    if [[ $simp_file != *.simp ]]; then
        echo "Error: Input file must have a .simp extension, but provided $simp_file"
        return 1
    fi
    local base_name="${simp_file%.simp}"
    echo "${base_name}.o"
}

print_help() {
    echo "flags:"
    echo "--dump\tUse objdump to dump the asm per object"
    echo "--log\tTell the compiler to log the CLIF IR"
    echo "--debug\tUse debug build for the compiler instead of release"
    echo "--use-lld\tUse lld linker for clang"
    echo "--no-run\tOnly compile the program"
}

for arg in $@; do
    if [[ $arg = "--help" ]]; then
        print_help
        exit 0
    elif [[ $arg = "--dump" ]]; then
        dump=true
    elif [[ $arg = "--log" ]]; then
        log_flag="--log"
    elif [[ $arg = "--debug" ]]; then
        release_flag=""
    elif [[ $arg = "--use-lld" ]]; then
        fuse_lld_flag="-fuse-ld=lld"
    elif [[ $arg = "--no-run" ]]; then
        run=false
    else
        sources+=($arg)
    fi
done

if [[ ${#sources[@]} == 0 ]]; then
    echo "no source files provided"
    exit 1
fi

objects=()

for source in ${sources[@]}; do
    out_path=$(simp_to_o $source)
    objects+=($out_path)
    echo "# $0: Compiling $source into $out_path"
    cargo run $release_flag -- $log_flag $source -o $out_path
    if [ $? -ne 0 ]; then
        exit $?
    fi
    if $dump; then
        objdump -Dr $out_path
    fi
done

echo "# $0: Linking"

clang $fuse_lld_flag ${objects[@]} -o program

if [[ $? -ne 0 ]]; then
    exit $?
fi

if $run; then
    echo "# $0: Executing \`./program\`"
    ./program
    echo "# $0: The program returned $?"
else
    echo "# Finished compiling \`program\`"
fi
