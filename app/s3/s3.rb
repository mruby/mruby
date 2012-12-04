
class Time
  def utc_offset
    9 * 60 * 60 # JST
  end
  def rfc2822
    sprintf('%s, %02d %s %0*d %02d:%02d:%02d ',
      RFC2822_DAY_NAME[wday],
      day, RFC2822_MONTH_NAME[mon-1], year < 0 ? 5 : 4, year,
      hour, min, sec) +
    if utc?
      '-0000'
    else
      off = utc_offset
      sign = off < 0 ? '-' : '+'
      sprintf('%s%02d%02d', sign, (off.abs / 60) / 60, (off.abs / 60) % 60)
    end
  end

  RFC2822_DAY_NAME = [
    'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'
  ]
  RFC2822_MONTH_NAME = [
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
  ]
end

module AWS
  class S3
    S3_ENDPOINT = "s3.amazonaws.com"
    S3_PORT = 80

    def initialize(access_key, secret_key)
      @access_key = access_key
      @secret_key = secret_key
    end

    def set_bucket(bucket_name)
      @bucket_name = bucket_name
    end

    def download(path)
      host = @bucket_name + "." + S3_ENDPOINT
      date = Time.now.rfc2822
      sig_encoded = sign(:get, date, path)
      header = {
        'Host' => host,
        'Date' => date,
        'Authorization' => "AWS " + @access_key + ":" + sig_encoded
      }

      req = create_http_request(:get, "", header)
      http = SimpleHttp.new(host, S3_PORT)
      http.request("GET", path, req)
    end

    def upload(path, text)
      host = @bucket_name + "." + S3_ENDPOINT
      date = Time.now.rfc2822
      sig_encoded = sign(:put, date, path)
      header = {
        'Host' => host,
        'Date' => date,
        'Authorization' => "AWS " + @access_key + ":" + sig_encoded
      }

      req = create_http_request(:put, text, header)
      http = SimpleHttp.new(host, S3_PORT)
      http.request("PUT", path, req)
    end

    # private
    def encode_parameters(params, delimiter = '&', quote = nil)
      if params.is_a?(Hash)
        params = params.map do |key, value|
          sprintf("%s=%s%s%s", escape(key), quote, escape(value), quote)
        end
      else
        params = params.map { |value| escape(value) }
      end
      delimiter ? params.join(delimiter) : params
    end

    def create_http_request(method, body, header)
      request = {}
      request['User-Agent'] = "Mozilla/5.0"
      if method == :post or method == :put
        request["body"] = body.is_a?(Hash) ? encode_parameters(body) : body.to_s
        request["Content-Type"] = "text/plain"
        request["Content-Length"] = (request["body"] || '').length
      end
      request.merge(header)
    end

    def base64(value)
      [ value ].pack('m').gsub(/\n/, '')
    end

    def sign(proto, date, path)
      sign_str = String.new
      case proto
      when :put
        sign_str += "PUT\n\n"
        sign_str += "text/plain\n"
      when :get
        sign_str += "GET\n\n\n"
      end
      sign_str += date + "\n"
      sign_str += "/" + @bucket_name + path

      digest = Digest::HMAC.digest(sign_str, @secret_key, Digest::SHA1)
      base64(digest).gsub("\n", '')
    end
  end
end
