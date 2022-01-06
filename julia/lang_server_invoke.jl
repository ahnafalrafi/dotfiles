using Pkg
Pkg.instantiate()
using LanguageServer
using SymbolServer
depot = get(ENV, "JULIA_DEPOT_PATH", "")
project = dirname(something(Base.current_project(pwd()),
                            Base.load_path_expand("@v#.#")))
# Make sure that we only load packages from this environment specifically.
@info "Running language server" env=Base.load_path()[1] pwd() project depot
server = LanguageServer.LanguageServerInstance(stdin, stdout, project, depot)
server.runlinter = true
run(server)
