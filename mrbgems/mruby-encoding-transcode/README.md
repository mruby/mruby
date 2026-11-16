# mruby-encoding-transcode

`String#encode`, CRuby's encoding errors and `Encoding.default_external`,
for a build that asks for them:

```ruby
conf.gem core: 'mruby-encoding-transcode'
```

It depends on mruby-encoding, whose strings are UTF-8 or binary.

## `String#encode` and `String#encode!`

```ruby
str.encode(dst = Encoding.default_internal, src = str.encoding, **opts)
str.encode!(dst = Encoding.default_internal, src = str.encoding, **opts)
```

They convert between UTF-8, ASCII-8BIT (`BINARY`) and US-ASCII, as CRuby
does:

```ruby
"aあ".encode("US-ASCII", undef: :replace)  # => "a?"
"a\xff".encode("UTF-8", invalid: :replace)     # => "a�"
"<a>".encode("UTF-8", xml: :text)              # => "&lt;a&gt;"
"a\nb".encode("UTF-8", crlf_newline: true)     # => "a\r\nb"
```

- A character the target cannot hold raises
  `Encoding::UndefinedConversionError`: one beyond ASCII from UTF-8 to
  ASCII-8BIT or US-ASCII, or a byte beyond ASCII from ASCII-8BIT.
- A byte that spells no character in the source raises
  `Encoding::InvalidByteSequenceError`. Converting a string to its own
  encoding leaves such bytes as they are, unless `invalid: :replace` is
  given; each maximal subpart counts once, as `String#scrub` counts it.
- Any other encoding raises `Encoding::ConverterNotFoundError`.
- With no `dst` and no `Encoding.default_internal`, the string is not
  converted, but the options still apply.

The options are CRuby's:

| option                  | what it does                                       |
| ----------------------- | -------------------------------------------------- |
| `invalid: :replace`     | replaces an invalid byte                           |
| `undef: :replace`       | replaces an undefined character                    |
| `replace: str`          | the replacement: U+FFFD for UTF-8, `"?"` otherwise |
| `fallback: obj`         | gives an undefined character's replacement         |
| `xml: :text` or `:attr` | escapes for XML text or an attribute value         |

`replace:` without `invalid: :replace` also replaces undefined characters, and
a replacement that is not well formed raises `ArgumentError`. `fallback:`
takes `obj[ch]`, or `obj.call(ch)` for a Proc or a Method, when neither `xml:`
nor `undef: :replace` handles the character first. `xml:`
escapes `&`, `<` and `>`, and `"` too for `:attr`, which quotes the result; an
undefined character becomes `&#xHH;`. Newlines convert with
`newline: :universal`, `:lf`, `:crlf` or `:cr`, or with `universal_newline:`,
`lf_newline:`, `crlf_newline:` or `cr_newline: true`.

Unlike CRuby:

- a result in US-ASCII is tagged ASCII-8BIT, since mruby tags every string
  that is not UTF-8 so, and such a string is read as ASCII-8BIT unless
  US-ASCII is given as `src`;
- there are no other encodings to convert, and no `Encoding::Converter`.

## The encoding errors

`EncodingError`, and under it `Encoding::CompatibilityError`,
`Encoding::ConverterNotFoundError`, `Encoding::InvalidByteSequenceError` and
`Encoding::UndefinedConversionError`.

- `InvalidByteSequenceError` has `#error_bytes`, `#source_encoding_name`,
  `#destination_encoding_name` and `#incomplete_input?`.
- `UndefinedConversionError` has `#error_char`, `#source_encoding_name` and
  `#destination_encoding_name`.
- Both have `#source_encoding` and `#destination_encoding`, which are names
  here, as every encoding is.

Nothing here raises `CompatibilityError`; it is for code that rescues it.

## `Encoding.default_external` and `Encoding.default_internal`

`default_external` is `Encoding::UTF_8` and `default_internal` is `nil`,
until they are set. They take the names mruby-encoding knows, and
`default_external` cannot be `nil`. `String#encode` with no target converts
to `default_internal`. mruby-io reads text as UTF-8 whatever they say.
