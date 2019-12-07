module Carbuncle
  module FreetypeDownloader
    FREETYPE_URL = 'https://download.savannah.gnu.org/releases/freetype/ft2101.zip'.freeze
    FREETYPE_SUBDIR = 'freetype-2.10.1'.freeze

    class << self
      def download(vendor_dir)
        puts('Downloading Freetype...')
        freetype_zip = File.join(vendor_dir, 'freetype.zip')
        freetype_dir = File.join(vendor_dir, 'freetype')

        unless File.exist?(freetype_zip)
          FileUtils.mkdir_p(vendor_dir)
          open(freetype_zip, 'wb') do |file|
            file << open(FREETYPE_URL).read
          end
        end

        FileUtils.mkdir_p(freetype_dir)

        Zip::File.open(freetype_zip) do |zip_file|
          zip_file.each do |f|
            fpath = File.join(freetype_dir, f.name)
            FileUtils.mkdir_p(File.dirname(fpath))
            zip_file.extract(f, fpath) unless File.exist?(fpath)
          end
        end
      end
    end
  end
end
