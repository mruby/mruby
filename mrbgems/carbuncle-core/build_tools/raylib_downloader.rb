module Carbuncle
  module RaylibDownloader
    class << self
      def download(build_dir)
        raylib_url = 'https://github.com/raysan5/raylib/archive/2.5.0.zip'
        vendor_dir = File.join(build_dir, 'vendor')
        raylib_zip = File.join(vendor_dir, 'raylib.zip')
        raylib_dir = File.join(vendor_dir, 'raylib')

        unless File.exist?(raylib_zip)
          FileUtils.mkdir_p(vendor_dir)
          open(raylib_zip, 'wb') do |file|
            file << open(raylib_url).read
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

        File.join(raylib_dir, 'raylib-2.5.0')
      end
    end
  end
end
