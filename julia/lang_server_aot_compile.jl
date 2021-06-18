using PackageCompiler
create_sysimage([:LanguageServer, :SymbolServer, :StaticLint, :Debugger],
                sysimage_path=string(Base.ENV["XDG_CACHE_HOME"],
                                     "/julia/julials.so"))
