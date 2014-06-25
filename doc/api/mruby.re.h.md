# mruby/re.h
`Regexp` class stuffs.
Since regexp was removed there is not much in mruby itself

## Regexp mrbgems.
Following is the implementations of regexp mrbgem in [mgem-list](https://github.com/mruby/mgem-list):
* [mruby-onig-regexp](https://github.com/mattn/mruby-onig-regexp)
  * Uses [oniguruma](http://www.geocities.jp/kosako3/oniguruma/) for regexp engine.
* [mruby-regexp-pcre](https://github.com/iij/mruby-regexp-pcre)
  * Uses [PCRE](http://www.pcre.org/) for regexp engine.
  * Doesn't need to install PCRE since it's included in the mrbgem.
* [mruby-pcre-regexp](https://github.com/mattn/mruby-pcre-regexp)
  * Uses PCRE for regexp engine.
  * Needs to install PCRE to use this.
* [mruby-hs-regexp](https://github.com/masamitsu-murase/mruby-hs-regexp)
  * Uses [Henry Spencer's regular expression libraries](http://www.arglist.com/regex).
  * Smallest regexp mrbgem.

## REGEXP_CLASS
```C
#define REGEXP_CLASS          "Regexp"
```
String literal of `Regexp` class name.
