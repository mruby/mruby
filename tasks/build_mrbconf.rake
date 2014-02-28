MRuby.each_target do |target|
  next if mrbconf.empty?

  mrbconf_h = "#{build_dir}/include/build_mrbconf.h"

  source = <<EOS
#ifndef MRB_BUILD_MRBCONF_H
#define MRB_BUILD_MRBCONF_H

#{mrbconf.map { |v| "#define MRB_#{v.to_s.upcase}" }.join("\n")}

#endif
EOS

  if not File.exists? mrbconf_h or File.read(mrbconf_h) != source
    FileUtils.mkdir_p File.dirname mrbconf_h
    File.write mrbconf_h, source
  end

  file "#{MRUBY_ROOT}/include/mrbconf.h" => mrbconf_h

  compilers.each do |v|
    v.include_paths << File.dirname(mrbconf_h)
    v.defines << 'MRB_HAVE_BUILD_MRBCONF_H'
  end
end
