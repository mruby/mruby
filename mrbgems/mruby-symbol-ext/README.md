# mruby-symbol-ext

This gem extends mruby's `Symbol` class with additional useful methods.

## Methods

Here are the methods added to the `Symbol` class:

### `capitalize`

- **call-seq:** `sym.capitalize  -> symbol`
- Returns a new symbol with the first character converted to uppercase and the remainder to lowercase. Equivalent to `sym.to_s.capitalize.intern`.

### `downcase`

- **call-seq:** `sym.downcase  -> symbol`
- Returns a new symbol with all characters converted to lowercase. Equivalent to `sym.to_s.downcase.intern`.

### `upcase`

- **call-seq:** `sym.upcase    -> symbol`
- Returns a new symbol with all characters converted to uppercase. Equivalent to `sym.to_s.upcase.intern`.

### `casecmp(other_symbol)`

- **call-seq:** `sym.casecmp(other)  -> -1, 0, +1 or nil`
- Performs a case-insensitive comparison between two symbols. Returns -1, 0, or +1 if `other_symbol` is a symbol. Returns `nil` if `other_symbol` is not a symbol.

### `casecmp?(other_symbol)`

- **call-seq:** `sym.casecmp?(other)  -> true, false, or nil`
- Returns `true` if the receiver and `other_symbol` are equal after case folding, `false` if they are not equal. Returns `nil` if `other_symbol` is not a symbol.

### `empty?`

- **call-seq:** `sym.empty?   -> true or false`
- Returns `true` if the symbol's string representation is empty (i.e., `:""`), `false` otherwise.

### `length` / `size`

- **call-seq:**
  - `sym.length    -> integer`
  - `sym.size      -> integer`
- Returns the length of the symbol's string representation. `size` is an alias for `length`.

### `Symbol.all_symbols` (Conditional)

- **call-seq:** `Symbol.all_symbols    => array`
- Returns an array of all symbols currently in mruby's symbol table.
- **Note:** This method is only available if mruby is compiled with the `MRB_USE_ALL_SYMBOLS` define.
