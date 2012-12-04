# Twitterクライアントの実行方法

 * OAuthの設定を twitter.rb に行なって下さい
   * https://dev.twitter.com にアクセスし、 "Create an app" のリンクをクリックして、アプリケーションを作成してください
   * Settings ペインからアプリケーションのアクセスレベルを "Read and Write" に変更してください。サンプルアプリケーションはメッセージの投稿を行うため Write アクセスを要求します。
   * Details ペイン最下部の "Create my access token" のリンクをクリックして、自分のアクセストークンを作成してください
   * Consumer key, Consumer secret, Access token, Access token secret の 4つの情報を twitter.rb に記述してください
 * sh run.sh を実行することで、テストメッセージをTwitterに投稿し、最新のツイートを取得することができます

# UNIX環境用に拡張したmrubyで使えるライブラリ

 * SimpleHttp: ::new #address #port #get #post
   * SimpleHttp::SimpleHttpResponse: ::new #[] #[]= #header #body
                #status #code #date #content_type #content_length #each #each_name
 * SimpleURI: ::new ::parse ::split #[] #[]= #request_uri #to_s
              #scheme #scheme= #userinfo #userinfo= #host #host=
              #port #port= #path #path= #query #query=
   * SimpleURI::Error < StandardError
   * SimpleURI::InvalidURIError < SimpleURI::Error
 * SimpleOAuth

# SimpleHttp

* Net::HTTP ライクな HTTPクライアントです
* シンプルに get / post のみ実装してあります

# SimpleURI

* URI ライクな URIパーサです
* 内部で正規表現を利用しています

# SimpleOAuth

* シンプルな OAuth クライアントです

