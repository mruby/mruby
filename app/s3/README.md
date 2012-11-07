# How to use AWS::S3

 * s3_upload.rb is an example script for AWS::S3 upload/download.
 * Rewrite AWSAccessKeyId, AWSSecretAccessKey and AWSBucket respectively

# AWS::S3

* Public Class Methods
  * new(access_key, secret_key) -> AWS::S3
    * Create a new AWS::S3 object
    * access_key: Access key for AWS
    * secret_key: Secret key for AWS

* Public Instance Methods
  * set_bucket(bucket_name) -> bucket_name
    * set bucket name to 'backet_name'
  * upload(path, text) -> SimpleHttpResponse
    * upload text to the path
  * download(path) -> SimpleHttpResponse
    * download object from the path
