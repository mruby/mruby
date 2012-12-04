$LOAD_PATH.unshift "./" + File.dirname(__FILE__)
require 'simple_http'
require 's3'

AWSAccessKeyId = '(AWS Access Key)'
AWSSecretAccessKey = '(AWS Secret Access Key)'
AWSBucket = "(Bucket name)"

aws = AWS::S3.new(AWSAccessKeyId, AWSSecretAccessKey)

aws.set_bucket(AWSBucket)

# PUT /Hellow.txt
response = aws.upload("/Hello.txt", "Hello! mruby " + Time.now.to_s)
if response.code.to_i == 200
  puts "Upload: success"
else
  puts "Error code = " + response.code.to_s
end

# GET /Hello.txt
response = aws.download("/Hello.txt")
if response.code.to_i == 200
  puts "Download: success"
  puts response.body
else
  puts "Error code = " + response.code.to_s
  puts "Response:"
  puts response.body
end
