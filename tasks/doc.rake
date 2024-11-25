MRuby.autoload :Documentation, 'mruby/doc'

desc 'generate document'
task :doc => %w[doc:api doc:capi]

namespace :doc do
  desc 'generate yard docs'
  task :api do
    begin
      sh "mrbdoc"
    rescue
      puts "ERROR: To generate YARD documentation, you should install the yard-coderay and yard-mruby gems."
      puts "  $ gem install yard-coderay yard-mruby"
      puts "https://yardoc.org/"
      puts "https://rubygems.org/gems/yard-mruby"
      puts "https://rubygems.org/gems/yard-coderay"
    end
  end

  desc 'generate doxygen docs'
  task :capi do
    begin
      sh "doxygen Doxyfile"
    rescue
      puts "ERROR: To generate C API documentation, you should install Doxygen and Graphviz."
      puts "On Debian-based systems:"
      puts "  $ sudo apt-get install doxygen graphviz"
      puts "On RHEL-based systems:"
      puts "  $ sudo dnf install doxygen graphviz"
      puts "On macOS-based systems:"
      puts "  $ brew install doxygen graphviz"
      puts "https://www.doxygen.nl/"
      puts "https://graphviz.org/"
    end
  end

  desc 'clean all built docs'
  task :clean => %w[clean:api clean:capi]

  namespace :clean do
    desc 'clean yard docs'
    task :api do
      rm_rf %w(doc/api .yardoc)
    end

    desc 'clean doxygen docs'
    task :capi do
      rm_rf 'doc/capi'
    end
  end

  namespace :view do
    desc 'open yard docs'
    task :api do
      if RUBY_PLATFORM.include?('darwin')
        sh 'open doc/api/index.html'
      else
        sh 'xdg-open doc/api/index.html'
      end
    end

    desc 'open doxygen docs'
    task :capi do
      if RUBY_PLATFORM.include?('darwin')
        sh 'open doc/capi/html/index.html'
      else
        sh 'xdg-open doc/capi/html/index.html'
      end
    end
  end

  desc 'update doc/internal/opcode.md'
  task 'update-opcode.md' do
    unless system(*%W(git --git-dir #{MRUBY_ROOT}/.git --work-tree #{MRUBY_ROOT} diff --quiet @ -- doc/internal/opcode.md))
      abort <<~'ERRMESG'
        The file "doc/internal/opcode.md" has been modified but not committed.
        To avoid loss of your edits, the automatic update process has been aborted.
      ERRMESG
    end

    MRuby::Documentation.update_opcode_md
  end

  task 'update-index' do
    rev_order = %w(doc/internal/ doc/guides/ doc/)
    cmd = %W(git --git-dir #{MRUBY_ROOT}/.git --work-tree #{MRUBY_ROOT} ls-files -- doc/*.md)
    doc = IO.popen(cmd, "r") { |io| io.read.split("\n") }
    doc.sort_by! { |e| [-rev_order.index { |o| e.start_with?(o) }, e] }
    readme_path = File.join(MRUBY_ROOT, "README.md")
    readme = File.read(readme_path)
    matched = false
    mark_begin = "<!-- BEGIN OF MRUBY DOCUMENT INDEX -->\n"
    mark_end = "<!-- END OF MRUBY DOCUMENT INDEX -->\n"
    readme1 = readme.sub(/^#{mark_begin}\n\K.*(?=^\n#{mark_end})/m) {
      matched = true
      doc.each_with_object("") { |d, a|
        summary = File.open(File.join(MRUBY_ROOT, d)) { |f|
          f.each_line.first.slice(/^<!--\s*summary:\s*(.*?)\s*-->/, 1)
        }
        if summary
          summary = "Internal Implementation / #{summary}" if d.start_with?("doc/internal/")
          a << "- [#{summary}](#{d})\n"
        end
      }
    }
    raise "missing marker for document index in README.md" unless matched
    File.write(readme_path, readme1, mode: "wb") unless readme == readme1
  end
end

# deprecated
task "api_doc" => "doc:api"
task "capi_doc" => "doc:capi"
task "clean_doc" => "doc:clean"
task "clean_api_doc" => "doc:clean:api"
task "clean_capi_doc" => "doc:clean:capi"
task "view_api" => "doc:view:api"
task "view_capi" => "doc:view:capi"
