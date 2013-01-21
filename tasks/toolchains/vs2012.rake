MRuby::Toolchain.new(:vs2012) do |conf|
  conf.cc do |cc|
    cc.command = ENV['CC'] || 'cl.exe'
    cc.flags = [ENV['CFLAGS'] || %w(/c /nologo /W3 /D_DEBUG /MDd /Zi /Od /RTC1 /DDISABLE_GEMS /DHAVE_STRING_H /DNO_GETTIMEOFDAY /D_CRT_SECURE_NO_WARNINGS)]
    cc.include_paths = ["#{root}/include"]
    cc.defines = %w(DISABLE_GEMS)
    cc.option_include_path = '-I%s'
    cc.option_define = '-D%s'
    cc.compile_options = "%{flags} /Fo%{outfile} -c %{infile}"
  end

  conf.linker do |linker|
    linker.command = ENV['LD'] || 'link.exe'
    linker.flags = [ENV['LDFLAGS'] || %w(/nologo)]
    linker.libraries = %w(kernel32 user32 gdi32 winspool comdlg32 advapi32 shell32 ole32 oleaut32 uuid odbc32 odbccp32)
    linker.library_paths = []
    linker.option_library = '-l%s'
    linker.option_library_path = '-L%s'
    linker.link_options = "%{flags} /OUT:%{outfile} %{objs} %{libs}"
  end
 
  conf.archiver do |archiver|
    archiver.command = ENV['AR'] || 'lib.exe'
    archiver.archive_options = '/OUT:%{outfile} %{objs}'
  end
 
  conf.yacc do |yacc|
    yacc.command = ENV['YACC'] || 'bison.exe'
    yacc.compile_options = '-o %{outfile} %{infile}'
  end
 
  conf.gperf do |gperf|
    gperf.command = 'gperf.exe'
    gperf.compile_options = '-L ANSI-C -C -p -j1 -i 1 -g -o -t -N mrb_reserved_word -k"1,3,$" %{infile} > %{outfile}'
  end

  conf.exts do |exts|
    exts.object = '.obj'
    exts.executable = '.exe'
    exts.library = '.lib'
  end

  conf.file_separator = '\\'
end
