#include <variant>

#include "ctll1.h"

using namespace ctll1;

//define nonterminals
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

//example implementation of a parsetree: as long as it satisfies the is_parsetree concept
//it should work (please report otherwise)

struct Nonterminal {
	NonterminalKind kind;
	std::string content;
};
using Token = ct<Kind, NonterminalKind>::Token;

//define a context for our definition of Kind/Nonterminalkind
using MyCt = ct<Kind, NonterminalKind>;

//unified terminal and nonterminals
struct AnyTerminal {
	AnyTerminal(Token t) :val{ t } {}
	AnyTerminal(Nonterminal t) :val{ t } {}

	AnyTerminal(Kind k) : val{ Token{k} } {}
	AnyTerminal(NonterminalKind k) : val{ Nonterminal{k} } {}


	bool isTerminal() const { return std::holds_alternative<Token>(val); }
	bool isNonTerminal() const { return std::holds_alternative<Nonterminal>(val); }

	const Token& getTerminal() const { return std::get<0>(val); }
	const Nonterminal& getNonTerminal() const { return std::get<1>(val); }

	bool isEqualKind(Kind k) const {
		return isTerminal() && getTerminal().kind == k;
	}

	bool isEqualKind(NonterminalKind k)const {
		return isNonTerminal() && getNonTerminal().kind == k;
	}

	std::string get_str_cont() const {
		return isNonTerminal() ? getNonTerminal().content : getTerminal().content;
	}

private:
	std::variant<Token, Nonterminal> val;
};

inline std::ostream& operator<< (std::ostream& os, const AnyTerminal& p) {
	if (p.isNonTerminal()) {
		os << p.getNonTerminal().kind;
		if (p.getNonTerminal().content != "")
			os << ": " << p.getNonTerminal().content;
	}
	else {
		os << p.getTerminal().kind;
		if (p.getTerminal().content != "")
			os << ": " << p.getTerminal().content;
	}
	return os;
}

struct ParseTree {
	AnyTerminal content;
	std::vector<ParseTree> children;

	ParseTree(NonterminalKind content, const std::vector<ParseTree>& children) noexcept
		: content{ AnyTerminal{content} }, children{ children } {
		length = std::accumulate(children.cbegin(), children.cend(), 0,
			[](auto a, auto b) {return a + b.length; });
	}
	ParseTree(Token content, const std::vector<ParseTree>& children)noexcept
		: content{ AnyTerminal{content} }, children{ children } {
		length = std::accumulate(children.cbegin(), children.cend(), 0,
			[](auto a, auto b) {return a + b.length; });
	}


	ParseTree(Token content) : content{ content }, children{ } {
		length = 1;
	}

	AnyTerminal getFstChildCont() const {
		return getContentChild(0);
	}

	std::string getStrContent() const {
		return content.isNonTerminal() ?
			content.getNonTerminal().content : content.getTerminal().content;
	}

	AnyTerminal getContentChild(int i) const {
		return children[i].content;
	}

	int getLength() const { return length; }
private:
	int length; //how many token the subtree ate.
};
inline std::ostream& printTree(std::ostream& os, const ParseTree& p, int level = 0) {
	for (int i = 0; i < level; i++) os << "  ";
	os << p.content << "\n";
	for (const auto& c : p.children) {
		printTree(os, c, level + 1);
	}
	return os;
}
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

	//transform it:
	constexpr auto expr_grammar = MyCt::transform_grammar(test_expr_grammar);

	//define an (already scanned) input stream:
	auto input_stream = std::vector<Token>{ {Kind::int_number,5},{Kind::plus}, {Kind::int_number,12}
	,{Kind::mult}, {Kind::int_number,23} };
	
	try {
		//parse the input stream.
		auto res = parse<Kind, NonterminalKind, ParseTree,
			expr_grammar, NonterminalKind::expression>(input_stream);
		printTree(std::cout, res);
	}
	catch (ParseError<Kind, NonterminalKind> e) {
		std::cout << "error at " << e.Where << " message: " <<
			e.message << " expected: " << e.expected << " actually there: " << e.there;
	}
}
