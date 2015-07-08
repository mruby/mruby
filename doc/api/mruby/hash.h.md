### mrb_hash_new

```C
mrb_value mrb_hash_new(mrb_state *mrb);
```

Initializes a hash.
#### Example

In this example we read from a Ruby file inside C. The Ruby code will print what you pass as an argument
and what class the passed in value is. This example initializes a hash. In pure Ruby doing this is equivalent
to Hash.new.

```C
#include <stdio.h>
#include <mruby.h>
#include "mruby/hash.h" // Needs the hash header.
#include "mruby/compile.h"


int main(int argc, char *argv[])
{   
    mrb_state *mrb = mrb_open();
    if (!mrb) { /* handle error */ }
    mrb_value new_hash; // Declare variable.
     FILE *fp = fopen("test_ext.rb","r");
     new_hash = mrb_hash_new(mrb);  // Initialize hash.
     mrb_value obj = mrb_load_file(mrb,fp);
     mrb_funcall(mrb, obj, "method_name", 1, new_hash);
     fclose(fp);
     mrb_close(mrb);
    return 0;
}
```

#### test_ext.rb

``` Ruby
class Example_Class
    def method_name(a)
        puts a
        puts a.class
    end
end
Example_Class.new
```

### mrb_hash_set

```C
#include <stdio.h>
#include <mruby.h>
#include "mruby/hash.h" // Needs the hash header.
#include "mruby/compile.h"


int main(int argc, char *argv[])
{   
    mrb_state *mrb = mrb_open();
    if (!mrb) { /* handle error */ }
     mrb_value new_hash; // Declare variable.
     mrb_sym hash_key = mrb_intern_cstr(mrb, "da_key"); // Declare a symbol.
     mrb_int hash_value = 80; // Declare a fixnum value.
     FILE *fp = fopen("test_ext.rb","r");
     new_hash = mrb_hash_new(mrb);  // Initialize hash.
     mrb_value obj = mrb_load_file(mrb,fp);
     mrb_hash_set(mrb, new_hash, mrb_symbol_value(hash_key), mrb_fixnum_value(hash_value)); // Set values to hash.
     mrb_funcall(mrb, obj, "method_name", 1, new_hash);
     fclose(fp);
     mrb_close(mrb);
    return 0;
}
```

#### test_ext.rb

```Ruby
class Example_Class
    def method_name(a)
        puts a
        puts a.class
    end
end
Example_Class.new
```

#### Result

After compiling you should get these results.

```Ruby
{:da_key=>80}
Hash
```


