require "pathname"

module MRuby
  module Source
    # mruby's source root directory
    ROOT = Pathname.new(File.expand_path('../../../',__FILE__))

    # Reads a constant defined at version.h
    MRUBY_READ_VERSION_CONSTANT = Proc.new do |name|
      ROOT.join('include','mruby','version.h').read.match(/^#define #{name} +"?([\w\. ]+)"?\r?$/)[1]
    end

    MRUBY_RUBY_VERSION = MRUBY_READ_VERSION_CONSTANT['MRUBY_RUBY_VERSION']
    MRUBY_RUBY_ENGINE = MRUBY_READ_VERSION_CONSTANT['MRUBY_RUBY_ENGINE']

    MRUBY_RELEASE_MAJOR = Integer(MRUBY_READ_VERSION_CONSTANT['MRUBY_RELEASE_MAJOR'])
    MRUBY_RELEASE_MINOR = Integer(MRUBY_READ_VERSION_CONSTANT['MRUBY_RELEASE_MINOR'])
    MRUBY_RELEASE_TEENY = Integer(MRUBY_READ_VERSION_CONSTANT['MRUBY_RELEASE_TEENY'])

    MRUBY_VERSION = [MRUBY_RELEASE_MAJOR,MRUBY_RELEASE_MINOR,MRUBY_RELEASE_TEENY].join('.')
    MRUBY_RELEASE_NO = (MRUBY_RELEASE_MAJOR * 100 * 100 + MRUBY_RELEASE_MINOR * 100 + MRUBY_RELEASE_TEENY)

    MRUBY_RELEASE_YEAR = Integer(MRUBY_READ_VERSION_CONSTANT['MRUBY_RELEASE_YEAR'])
    MRUBY_RELEASE_MONTH = Integer(MRUBY_READ_VERSION_CONSTANT['MRUBY_RELEASE_MONTH'])
    MRUBY_RELEASE_DAY = Integer(MRUBY_READ_VERSION_CONSTANT['MRUBY_RELEASE_DAY'])
    MRUBY_RELEASE_DATE = [MRUBY_RELEASE_YEAR,MRUBY_RELEASE_MONTH,MRUBY_RELEASE_DAY].join('.')

    # A commit hash as `git rev-parse` writes one, and nothing else. What
    # `.revision` holds is read against this before it is believed: a checkout
    # reads back the unexpanded `$Format:%H$` placeholder there.
    MRUBY_REVISION_RE = /\A[0-9a-f]{40}\z/

    # What tells `git` to answer for a repository other than the one it was
    # pointed at. A caller can have them set for a repository of its own (a
    # hook, a `git rebase --exec`, a `git bisect run`), and `-C` does not
    # outrank them, so the tree we ask about would not be the tree we build.
    MRUBY_GIT_REDIRECTS = {'GIT_DIR' => nil, 'GIT_WORK_TREE' => nil,
                           'GIT_COMMON_DIR' => nil, 'GIT_INDEX_FILE' => nil}

    # Asks the repository the source sits in which commit it is at. Nil where
    # there is none to ask: an unpacked archive carries no repository, a tree
    # copied out of one carries no history, git itself may not be installed,
    # and git refuses a repository owned by another user unless that user says
    # otherwise (`safe.directory`).
    #
    # A tree with no `.git` of its own is one of those, even where a
    # repository holds the directory above it: mruby vendored into a project
    # is not at the project's commit, and answering with it would name a
    # revision this source never sat at.
    MRUBY_READ_GIT_REVISION = Proc.new do
      if ROOT.join('.git').exist?
        # An argument list rather than a command line, so that a path with a
        # space in it needs no quoting of its own; `git` answers on stderr
        # where it cannot read a revision, and no answer is what we report.
        rev = IO.popen(MRUBY_GIT_REDIRECTS,
                       ['git', '-C', ROOT.to_s, 'rev-parse', '--verify', 'HEAD'],
                       err: File::NULL, &:read).to_s.strip
        rev if $?.success? && MRUBY_REVISION_RE =~ rev
      end
    rescue SystemCallError
      nil
    end

    # Reads the revision an archive was made from. `.revision` carries the
    # `$Format:%H$` placeholder in the repository and the commit hash in what
    # `git archive` writes out of it (see `.gitattributes`), so a source
    # release knows what it was cut from where a checkout would ask git.
    MRUBY_READ_ARCHIVE_REVISION = Proc.new do
      rev = ROOT.join('.revision').read.strip rescue nil
      rev if rev && MRUBY_REVISION_RE =~ rev
    end

    # The revision the source tree sits at, as a whole commit hash, or "" where
    # neither the repository nor the archive it was unpacked from could say.
    # What a build makes of the empty answer is `mruby/version.h`'s to say.
    MRUBY_FULL_REVISION = MRUBY_READ_GIT_REVISION.call ||
                          MRUBY_READ_ARCHIVE_REVISION.call || ""

    # The same revision abbreviated, which is what a version string can name it
    # with. The whole hash is what identifies the commit; ten characters are
    # what a reader can hold on to.
    MRUBY_REVISION = MRUBY_FULL_REVISION[0, 10]

    MRUBY_BIRTH_YEAR = Integer(MRUBY_READ_VERSION_CONSTANT['MRUBY_BIRTH_YEAR'])

    MRUBY_AUTHOR = MRUBY_READ_VERSION_CONSTANT['MRUBY_AUTHOR']
  end
end
