class MRBDoc
  DOC_DIR = 'language'

  def write_documentation dir, &block
    block.call "MRBDOC\tStart Building Documentation to #{doc_dir(dir)}"

    write(dir) do |progress|
      block.call progress
    end

    block.call "MRBDOC\tFinish Building Documentation"
  end

  private

  def write dir
    # io = STDOUT
    File.open(File.expand_path('Core_Classes.md', dir), 'w+') do |io|
      print_core_classes(io)
      print_core_modules(io)
    end
  end

  def get_core_list id
    core_list = {}
    each_file do |file_path, mrb_file|
      mrb_file.send(id) do |name, cls_hsh|
        core_list[name] = {:data => cls_hsh, :methods => {}, :class_methods => {}}
        mrb_file.each_method name do |met_name, met_hsh|
          core_list[name][:methods][met_name] = met_hsh
        end
        mrb_file.each_class_method name do |met_name, met_hsh|
          core_list[name][:class_methods][met_name] = met_hsh
        end
      end
    end
    core_list
  end

  def print_core_classes(io)
    core_list = get_core_list :each_class
    io.puts "# Core Classes\n\n"
    core_list.sort.each do |name, hsh|
      file = find_c_file_by_class(name)
      file = file.split("#{@dir}/")[1]
      iso = hsh[:data][:iso]
      iso = 'n/a' if iso.nil?

      io.puts <<CLASS
## #{name}

ISO Code | Mixins | Source File
--- | --- | ---
#{hsh[:data][:iso]} |  n/a | #{file}

CLASS
      print_class_methods(io, hsh)
      print_methods(io, hsh)
    end
  end

  def print_core_modules(io)
    core_list = get_core_list :each_module
    io.puts "# Core Modules\n\n"
    core_list.sort.each do |name, hsh|
      file = find_c_file_by_module(name)
      file = file.split("#{@dir}/")[1]
      iso = hsh[:iso]
      iso = 'n/a' if iso.nil?

      io.puts <<CLASS
## #{name}

ISO Code | Source File
--- | --- 
#{iso} | #{file}

CLASS
      print_class_methods(io, hsh)
      print_methods(io, hsh)
    end
  end

  def print_methods(io, hsh)
    return unless hsh[:methods].size > 0
    io.puts "### Methods\n\n"
    hsh[:methods].sort.each do |met_name, met_hsh|
      print_method(io, met_name, met_hsh)
    end
  end

  def print_class_methods(io, hsh)
    return unless hsh[:class_methods].size > 0
    io.puts "### Class Methods\n\n"
    hsh[:class_methods].sort.each do |met_name, met_hsh|
      print_method(io, met_name, met_hsh)
    end
  end

  def print_method(io, met_name, met_hsh)
    line_no = find_c_func(met_hsh[:c_func])[:line_no]
    file = find_c_file(met_hsh[:rb_class], met_hsh[:c_func])
    file = file.split("#{@dir}/")[1]

    io.puts <<METHOD
#### #{met_name}

ISO Code | Source File | C Function | Line
--- | --- | ---
#{met_hsh[:iso]} | #{file} | #{met_hsh[:c_func]} | #{line_no}

METHOD
  end

  def doc_dir dir; File.expand_path DOC_DIR, dir; end
end
