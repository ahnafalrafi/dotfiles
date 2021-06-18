server_pkgpath = Base.find_package("LanguageServer")
if server_pkgpath == nothing
    exit()
else
    print(string(server_pkgpath, "\n"))
end
