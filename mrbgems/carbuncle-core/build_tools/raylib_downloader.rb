module Carbuncle
  module RaylibDownloader
    RAYLIB_URL = 'https://github.com/raysan5/raylib/archive/master.zip'.freeze
    RAYLIB_SUBDIR = 'raylib-master'.freeze

    class << self
      def download(vendor_dir)
        puts('Downloading Raylib...')
        raylib_zip = File.join(vendor_dir, 'raylib.zip')
        raylib_dir = File.join(vendor_dir, 'raylib')

        unless File.exist?(raylib_zip)
          FileUtils.mkdir_p(vendor_dir)
          open(raylib_zip, 'wb') do |file|
            file << open(RAYLIB_URL).read
          end
        end

        FileUtils.mkdir_p(raylib_dir)

        Zip::File.open(raylib_zip) do |zip_file|
          zip_file.each do |f|
            fpath = File.join(raylib_dir, f.name)
            FileUtils.mkdir_p(File.dirname(fpath))
            zip_file.extract(f, fpath) unless File.exist?(fpath)
          end
        end

        puts('Raylib downloaded')
      end
    end
  end
end
