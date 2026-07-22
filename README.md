# SmileScript

A simple scripting language running on a bytecode virtual machine. Inspired by [Lox](https://github.com/munificent/craftinginterpreters).

## Usage

```bash
cargo build --release
```

```bash
./target/release/smsc <FILE>
```

## Example

```sms
class Scone {
    topping(first, second) {
        print "scone with " + first + " and " + second;
    }
}
var scone = Scone();
scone.topping("berries", "cream");

class Nested {
    method() {
        fun function() {
            print this;
        }
        function();
    }
}
Nested().method();

class CoffeeMaker {
    init(coffee) {
        this.coffee = coffee;
    }
    brew() {
        print "Enjoy your cup of " + this.coffee;
        this.coffee = nil;
    }
}

var maker = CoffeeMaker("coffee and chicory");
maker.brew();

class Doughnut {
    cook() {
        print "Dunk in the fryer.";
        this.finish("sprinkles");
    }
    finish(ingredient) {
        print "Finish with " + ingredient;
    }
}

class Cruller : Doughnut {
    finish(ingredient) {
        super.finish("icing");
    }
}

var cruller = Cruller();
cruller.finish("salt");
```

## LICENSE
MIT
