server_pkgpath = Base.find_package("LanguageServer")
if server_pkgpath == nothing
    exit()
else
    log_fname = get(ENV, "XDG_CACHE_HOME", "") * "/julia/lang_server_locate"
    logfile = open(log_fname, "w")
    write(logfile, server_pkgpath)
    flush(logfile)
    close(logfile)
    print(server_pkgpath * "\n")
end
