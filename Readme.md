# CTLL1: Compile-Time Parser Generator for c++20
CTLL1 is a single header library to generate ll(1) parsers.
## Installation
As it is a self contained (only dependencies to standard library) single-header-library, simply copy the `ctll1.h` into your repository and include it:
```
#include "ctll1.h"
```
Be aware that you need a compiler with c++20 support.

## Compiler Support:
- GCC 13.2.0+
- Clang 16.0.6+
- MSVC 19.29+

## Quick Start
As a simple example, we will generate a parser for the classic expression grammar.

The full code of this example can be found in `/examples/expression_grammar_example.cpp`

To start, we need to include the library. For less typing, we will also `using namespace` the `ctll1` namespace:
```
#include "ctll1.h"
#include <variant>

using namespace ctll1;
```

Next, we need to define the set of terminals and nonterminals:
```
MAKE_ENUM(NonterminalKind,
	expression,
	operation,
	arithm_expr,
	factor
);

//define terminals
MAKE_ENUM(Kind,
	end_of_file, //parser needs this for error handling
	identifier,

	//literal values:
	int_number,
	float_number,

	//operators:
	plus,
	minus,
	mult,
	div,

	lparenthesis, //(
	rparenthesis //)
);
```

We use here the MakeEnum macro to supply `to_string(..)` methods for the `enum` as well as `<<` operator support.
Next, we define a `ParseTree` type. As long as it satisfies `ct<..>::is_parse_tree`

```
struct ParseTree {
	AnyTerminal content;
	std::vector<ParseTree> children;

	...

	int getLength() const { return length; }
private:
	int length; //how many token the subtree consumed.
};

```

Here I decided to create a costum type for the union of the sets of terminals an nonterminals, called `AnyTerminal`:

```
//unified terminal and nonterminals
struct AnyTerminal {
	[convenience methods]
private:
	std::variant<Token, Nonterminal> val;
};
```

Next, we define the actual grammar:

```
int main() {
	constexpr bool is_parse_tree = MyCt::is_parse_tree<ParseTree>();
	static_assert(is_parse_tree, "parsetree implementation does not satisfy constraint");
	//define the grammar:
	constexpr auto test_expr_grammar = ct_tuple{
		Rule{NonterminalKind::expression,
					Seq{NonterminalKind::operation,
						Alt{Seq{Kind::plus, NonterminalKind::expression},
								Seq{Kind::minus, NonterminalKind::expression},
								epsilon{}}}},
		Rule{NonterminalKind::operation,
			Seq{NonterminalKind::factor,
				Alt{Seq{Kind::mult, NonterminalKind::expression},
						Seq{Kind::div, NonterminalKind::expression},
						epsilon{}}} },
		Rule{NonterminalKind::factor,
			Alt{
				Seq{Kind::lparenthesis, NonterminalKind::expression, Kind::rparenthesis},
				Kind::identifier,
				Kind::int_number
			}
		}
	};
	...

```

We could also decide to not use any of the `Alt` or `Many` types, but they in general make
left factoring more readable:

### Alt
The production
```
Rule{ NonterminalKind::factor
	Alt{
		Kind::identifier,
		Kind::int_number
	}
}
```
means that `factor` can either be expanded to `Kind::identifier` or `Kind::identifier`. It is implemented
in `transform_grammar` to emit two productions:
```
Rule{ NonterminalKind::factor, Kind::identifier }
Rule{ NonterminalKind::factor, Kind::int_number }
```
Note: this step will require the introduction of new nonterminals, which happens automatically.
they will also be automatically removed in the resulting parse tree.

### Seq
Does what the name implies: expects a sequence of Nonterminals, Terminals or Alt, Many, Seq:

```
Rule{NonterminalKind::factor, Seq{Kind::int_number, Kind::int_number}}
```
Will expand a factor into two `int_number`s

### Many
Expects one or more occurences of a Seq, Alt, Many or any terminal or nonterminal:
```
Rule{ NonterminalKind::int_number
	Many{
		Kind::int_digit
	}
}
```
means that `int_number` can be expanded into zero or more `int_digits`. Transform rules will expand
it into:
```
Rule{ NonterminalKind::int_number, Kind::int_digit }
Rule{ NonterminalKind::int_number, epsilon }
```

Note: The grammar must not be left-recursive and also needs to be left-factored. Otherwise the library won't compile.

As a next step we will transform the grammar, meaning Many and Alt will be expanded into multiple productions:
```
//transform it:
constexpr auto expr_grammar = MyCt::transform_grammar(test_expr_grammar);

```

Next, we will create an simple (already scanned) token stream:

```
auto input_stream = std::vector<Token>{ {Kind::int_number,5},{Kind::plus}, {Kind::int_number,12}
,{Kind::mult}, {Kind::int_number,23} };
	
```

And finally:
```
auto res = parse<Kind, NonterminalKind, ParseTree,^expr_grammar, NonterminalKind::expression>
	(input_stream);
printTree(std::cout, res);

```

You shoud see the following output:

```
expression
  operation
    factor
      int_number
  plus
  expression
    operation
      factor
        int_number
      mult
      expression
        operation
          factor
            int_number

```

## Error generation
If an error occurs, it will throw an `ParseError<Kind,NonterminalKind>` including where in the inputstream the error occured as well as
the expected token and a brief error message. This feature is however still experimental.

## Size of grammar
With the `MSVC` I was able to parse a c-like grammar with ~5 levels of precedence, procedures, loops etc. and never hit limits.
Sadly however the `gcc` and `clang` it generated too long symbol names and didn't compile.
For smaller grammars it works on all three compilers.