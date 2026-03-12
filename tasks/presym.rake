all_prerequisites = ->(task_name, prereqs) do
  Rake::Task[task_name].prerequisites.each do |prereq_name|
    next if prereqs[prereq_name]
    prereqs[prereq_name] = true
    all_prerequisites.(Rake::Task[prereq_name].name, prereqs)
  end
end

MRuby.each_target do |build|
  presym = build.presym

  include_dir = "#{build.build_dir}/include"
  build.compilers.each{|c| c.include_paths << include_dir}
  build.gems.each{|gem| gem.compilers.each{|c| c.include_paths << include_dir}}

  prereqs = {}
  ppps = []
  build_dir = "#{build.build_dir}/"
  mrbc_build_dir = "#{build.mrbc_build.build_dir}/" if build.mrbc_build
  build.products.each{|product| all_prerequisites.(product, prereqs)}
  prereqs.each_key do |prereq|
    next unless File.extname(prereq) == build.exts.object
    next unless prereq.start_with?(build_dir)
    next if mrbc_build_dir && prereq.start_with?(mrbc_build_dir)
    ppp = prereq.ext(build.exts.presym_preprocessed)
    if Rake.application.lookup(ppp) ||
       Rake.application.enhance_with_matching_rule(ppp)
      ppps << ppp
    end
  end

  file presym.list_path => ppps do
    presyms = presym.scan(ppps)
    current_presyms = presym.read_list if File.exist?(presym.list_path)
    if presyms != current_presyms
      mkdir_p presym.header_dir
      %w[id table].each do |type|
        presym.send("write_#{type}_header", presyms)
      end
      presym.write_list(presyms)
    end
  end

  # Ensure .o files depend on presym headers being generated.
  # This is critical when a build's .o files are compiled during another
  # build's presym scanning chain (before :gensym completes), e.g.:
  #   - internal sub-builds (mrbc) triggered by their parent build
  #   - the implicit host build triggered by a cross build needing mrbc
  prereqs.each_key do |prereq|
    next unless File.extname(prereq) == build.exts.object
    next unless prereq.start_with?(build_dir)
    next if mrbc_build_dir && prereq.start_with?(mrbc_build_dir)
    file prereq => presym.list_path
  end

  task gensym: presym.list_path
end
