require 'digest'
require 'rbconfig'
require "#{MRUBY_ROOT}/tools/unicode/case_data"

# The tables are committed, so a build never reaches any of this, which is why
# what only one task needs is required inside it rather than up here. What runs
# here is what a Unicode version bump comes to: change `VERSION` in
# tools/unicode/case_data.rb, fetch the database it names, record the digests
# the fetch prints in `CHECKSUMS` beside it, and regenerate every table at
# once so that no build is left reading an older Unicode than its neighbour.
UNICODE_DATA_DIR = Unicode::CaseData.dir
UNICODE_FILES = Unicode::CaseData::FILES.map { |f| "#{UNICODE_DATA_DIR}/#{f}" }

UNICODE_GENERATORS = {
  'core' => ['tools/gen_unicase.rb', 'src'],
  'gem:regexp' => ['mrbgems/mruby-regexp/tools/gen_cased.rb', 'mrbgems/mruby-regexp/src'],
}

# The database is not in the repository, so each file is a task that fetches
# it. Asking for a table before anything was downloaded and asking for the
# download reach the same rule that way.
#
# The digest of what came down is printed rather than checked, because a bump
# fetches a release before anything can know its digests. What it prints, as
# `sha256sum` spells it, is the digest `CHECKSUMS` is then to record.
UNICODE_FILES.each do |path|
  file path do
    require 'open-uri'
    url = "#{Unicode::CaseData::URL_BASE}/#{File.basename(path)}"
    puts "downloading #{url}"
    mkdir_p File.dirname(path)
    File.binwrite("#{path}.tmp", URI.parse(url).open(&:read))
    mv "#{path}.tmp", path
    puts "  #{Digest::SHA256.file(path).hexdigest}  #{File.basename(path)}"
  end
end

def unicode_generate(script, outdir)
  sh RbConfig.ruby, "#{MRUBY_ROOT}/#{script}", "#{MRUBY_ROOT}/#{outdir}", UNICODE_DATA_DIR
end

namespace :unicode do
  desc "download the Unicode #{Unicode::CaseData::VERSION} character database"
  task :download => UNICODE_FILES

  desc 'generate all Unicode tables'
  task :generate => UNICODE_GENERATORS.keys.map { |name| "generate:#{name}" }

  namespace :generate do
    UNICODE_GENERATORS.each do |name, (script, outdir)|
      desc "generate the Unicode tables in #{outdir}"
      task name => UNICODE_FILES do
        unicode_generate(script, outdir)
      end
    end
  end
end
