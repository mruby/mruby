# mruby-eval

This mrbgem provides methods for evaluating Ruby code from strings in mruby.

## Features

The `mruby-eval` gem implements the following methods:

- **`eval`**: Evaluates a string as Ruby code. This is a private method on `Kernel`.
- **`instance_eval`**: Evaluates a string or a block within the context of an object. This is a method on `BasicObject`.
- **`class_eval` / `module_eval`**: Evaluates a string or a block within the context of a class or module. This is a method on `Module`.
- **`Binding#eval`**: Evaluates a string within the context of a `Binding` object.

## Usage

For detailed usage and examples, please refer to the standard Ruby documentation for these methods. The mruby implementation aims to be compatible with the behavior of these methods in CRuby.

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License

mruby-eval is released under the MIT License. See the [LICENSE](../../LICENSE) file in the main mruby repository for details.
