#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require 'simple_uri'
require 'simple_http'
require 'simple_oauth'

CONSUMER_KEY        = 'YOUR_CONSUMER_KEY'
CONSUMER_SECRET     = 'YOUR_CONSUMER_SECRET'
ACCESS_TOKEN        = 'YOUR_ACCESS_TOKEN'
ACCESS_TOKEN_SECRET = 'YOUR_ACCESS_TOKEN_SECRET'

simple_oauth = SimpleOAuth.new(CONSUMER_KEY, CONSUMER_SECRET, ACCESS_TOKEN, ACCESS_TOKEN_SECRET)

# post tweet
puts "#========== post tweet ============"
msg = sprintf("test %s ", Time.now.to_s)
ponse = simple_oauth.post('http://api.twitter.com/1.1/statuses/update.json', {
  :status => msg
})
if response.code.to_i == 200
  # puts response.body
  puts sprintf("メッセージ: '%s' をツイートしました！！", msg)
else
  puts sprintf("メッセージ: '%s' のツイートに失敗しました ＞＜", msg)
  puts response.body
  raise "Request failed: " + response.code.to_s
end
puts "#=================================="


# get timeline
puts "#========== get timeline =========="
response = simple_oauth.get('http://api.twitter.com/1.1/statuses/user_timeline.json?count=1')
puts "#response"
puts "Status: " + response["status"]
puts "Content-Type: " + response["content-type"]
puts "Content-Length: " + response["content-length"]
if response.code.to_i == 200
  puts response.body
  puts sprintf("%d bytes fetched.", response.body.size)
else
  puts response.body
  raise "Request failed: " + response.code.to_s
end

