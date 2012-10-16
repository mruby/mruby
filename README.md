# このリポジトリについて

mruby に対して、主に UNIX 環境に依存する機能を追加した fork です。


# 拡張部分

 * 追加したクラス/モジュール
   * Digest::MD5/RIPEMD160/SHA1/SHA256/SHA384/SHA512
   * Digest::HMAC: #reset 以外
   * ENV: ::[] ::[]= ::clear ::delete ::inspect ::keys ::size ::store
          ::to\_a ::to\_hash ::to\_s ::values
   * Errno::EXXX
   * File: ::open ::umask ::unlink ::delete ::rename ::exist? ::exists?
           ::dirname #path
   * IO: ::open ::sysopen ::popen ::select
         #close #closed? #each #each_byte #each_line #read #sync #sync=
         #write #to_io
   * IPAddr: ::new #<=> #family #hton #inspect #ipv4? #ipv6? #mask #to\_s #| #~
   * Regexp: ::compile ::last\_match #match
   * TCPSocket: ::new ::open
   * UNIXSocket: #addr ::new ::open #peeraddr
   * Syslog: ::open ::close ::log ::opened? ::ident ::options ::facility
   * SystemCallError

 * 拡張したクラス/モジュール
   * Array: #- #& #| #pack #uniq #uniq! #flatten #flatten!
   * Kernel: #exit #load #require #sleep #system
     * load, require については https://github.com/iij/mruby/wiki/require も参照してください
   * String: #lstrip #rstrip #strip #unpack #gsub #gsub! #sub #sub! #scan
     * Array#pack, String#unpack で利用できるテンプレート文字列は "m"(base64) 、"H"(16進文字列/上位ニブルが先)と"C"(8bit 符号なし整数) のみです。

 * その他の拡張


# ブランチ

 * master : mruby/mruby の master と同じです。毎日同期します。
 * iij : 主な開発ブランチです。master (=mruby/mruby) をベースに
   UNIX プラットホームに依存した拡張を含みます。
 * pr-hogehoge : master から分岐した、pull-request 送信専用のブランチです。
   ひとつの pull-request に対してひとつの pr-hogehoge ブランチを作成します。

# 機能拡張に関連するテスト

 * UNIX 環境に依存する機能のテストコードは、test/posix ディレクトリに追加しています。
 * 以下のようにコマンドを実行することで、テストを実施することができます。

> $ cd test/posix # test/posix ディレクトリへ移動
> 
> $ sh all.sh     # test/posix 以下にある全てのテストを実行

# ライセンス

オリジナル mruby と同じです。

# 謝辞
 * 正規表現リテラルの実装には、以下のレポジトリのコードを一部流用させていただきました。
    * https://github.com/junjis0203/mruby
