/*
* Under BSD 3-Clause License:
*
* Copyright 2023 Benjamin Dütschler
* Redistribution and use in source and binary forms, with or without modification,
* are permitted provided that the following conditions are met:
* 1. Redistributions of source code must retain the above copyright notice,
* this list of conditions and the following disclaimer.
* 2. Redistributions in binary form must reproduce the above copyright notice,
* this list of conditions and the following disclaimer in the documentation and/or other
* materials provided with the distribution.
* 3. Neither the name of the copyright holder nor the names of its contributors
* may be used to endorse or promote products derived from this software without specific
* prior written permission.
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
* OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
* IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
* INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
* PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
* HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
* OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
* THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/*
* For a tutorial how to use this header, please refer to the Readme.md
* and for full examples refer to the examples/ folder.
*/
#ifndef COMPILE_TIME_LL1_PARSER_GENERATOR
#define COMPILE_TIME_LL1_PARSER_GENERATOR

#include <vector>
#include <functional>
#include <variant>
#include <algorithm>
#include <vector>
#include <variant>
#include <span>
#include <iostream>
#include <numeric>
#include <string>
#include <type_traits>
#include <stdexcept>
//simple macro to supply enum with to_string method and ostream operator.
// 
// how to use:
// MAKE_ENUM(enum_name, fstmem,sndmem,...);
// now to_string(enum_name) and cout << enum_name is valid and will
// output the actual name instead of a number.
// 
#define MAKE_ENUM(name, ...) enum class name { __VA_ARGS__, __COUNT}; \
inline std::string to_string(name value) {\
static std::vector<std::string> strings = []() {\
std::vector<std::string> res; \
std::string temp;\
for (int i = 0; #__VA_ARGS__[i]; i++) {\
    if (#__VA_ARGS__[i] == ',') { res.push_back(temp); temp = "";continue; }\
    if (isspace(#__VA_ARGS__[i])) continue; \
       temp += #__VA_ARGS__[i]; \
} \
res.push_back(temp); \
return res; \
}();\
return strings[static_cast<int>(value)]; \
}\
inline std::ostream& operator<<(std::ostream& os, name value) { \
os << to_string(value);\
return os;} \

namespace ctll1 {


// concepts to disambiguate the template functions
// Note: is_seq e.g. doesn't really check wheter
//		 it is a sequence, only wheter it has the 
//		 members of a sequence to simplify code.
//-----------------------------------------------
template <typename T>
concept is_seq = requires {
	T::rep;
};

template <typename T>
concept is_rule = requires {
	T::lhs;
	T::rhs;
};

template<typename T>
concept is_many = requires {
	T::many;
}; 

template<typename T>
concept is_pair = requires {
	T::first; T::second;
};

template<typename T>
concept is_compound_tuple = requires {
	T::rest;
};

//represents the empty word:
struct epsilon {
	constexpr bool operator==(epsilon) const { return true; }
	explicit operator int() const {
		return -1;
	}
};


//represents an empty tuple
// 
//seperate class from ct_tuple, because 
//ct_tuple is constructed as [first, rest] pair
//and therefore can't be empty
struct empty_tuple {
	constexpr int size() const { return 0; }
	constexpr bool operator==(empty_tuple) const { return true; }
};

/*
* ct_tuple is a compile time tuple, meaning that it 
* isn't restricted to be used as template parameter
* e.g. parse<ct_tuple{...}>(...). the std::tuple is 
* unfortunatly not suited for that kind of use.
*/
template <typename T, typename ... Ts>
struct ct_tuple {
	T fst;
	ct_tuple<Ts...> rest;

	constexpr ct_tuple(T fst, Ts ... rest) : fst{ fst }, rest{ rest... } {}
	constexpr ct_tuple(T fst, ct_tuple<Ts...> rest) : fst{ fst }, rest{ rest } {}
	explicit constexpr ct_tuple() : fst{}, rest{} {}

	template <typename S, typename...Ss>
	constexpr ct_tuple<S, Ss...> create_ct_tuple(S fst, ct_tuple<Ss...> rest) const {
		return ct_tuple<S, Ss...>{fst, rest};
	}

	constexpr bool operator== (const ct_tuple<T, Ts...>& c) const {
		return fst == c.fst && rest == c.rest;
	}

	consteval int size() const { return 1 + sizeof...(Ts); }

	template<typename... Tss>
	constexpr ct_tuple<T, Ts..., Tss...> concatenate(const ct_tuple<Tss...>& tuple) const {
		return { fst,rest.concatenate(tuple) };
	}

	template<int Where, typename S>
	consteval auto replace(const S& to_replace) const {
		if constexpr (Where == 0)
			return ct_tuple<S, Ts...>{ to_replace, rest };
		else
			return create_ct_tuple(fst, rest.template replace<Where - 1>(to_replace));
	}

	template<typename... Tss>
	constexpr ct_tuple<T, Ts..., Tss...> operator+ (ct_tuple<Tss...> tuple) const {
		return concatenate(tuple);
	}

	constexpr ct_tuple<T, Ts...> operator+ (empty_tuple) const {
		return *this;
	}

	template <auto callable>
	constexpr auto map() const {
		return ct_tuple<decltype(callable(fst))>{ callable(fst) } + rest.template map<callable>();
	}

	template <auto callable, typename S>
	consteval auto fold(S startstate) const {
		return rest.template fold<callable>(callable(fst, startstate));
	}

};

template <typename T>
struct ct_tuple<T> {
	T fst;
	constexpr ct_tuple(T fst) : fst{ fst } {}
	constexpr ct_tuple() : fst{} {}

	constexpr bool operator== (ct_tuple<T> c) const {
		return fst == c.fst;
	}
	template<typename... Ts>
	constexpr ct_tuple<T, Ts...> operator+ (ct_tuple<Ts...> tuple) const {
		return ct_tuple<T, Ts...>{fst, tuple};
	}
	constexpr ct_tuple<T> operator+ (empty_tuple) const {
		return *this;
	}

	static consteval int size() { return 1; }

	template<typename... Tss>
	constexpr ct_tuple<T, Tss...> concatenate(const ct_tuple<Tss...>& tuple) const {
		return { fst,tuple };
	}
	template<int Where, typename S>
	consteval auto replace(const S& to_replace) const {
		if constexpr (Where == 0)
			return ct_tuple<S>{ to_replace };
		else
			return *this;
	}

	template <auto callable>
	constexpr auto map() const {
		return ct_tuple<decltype(callable(fst))>{ callable(fst) };
	}

	template <auto callable, typename S>
	consteval auto fold(S startstate) const {
		return callable(fst, startstate);
	}

};

template <typename ...Ts>
static constexpr ct_tuple<Ts...> operator+ (empty_tuple, ct_tuple<Ts...> o) {
	return o;
}


//deep first returns the the first non-pair element of nested pairs:
// i.e.
//deep_first(pair{pair{1,3},4}) == 1
template <typename T>
constexpr auto deep_first(T finish) {
	return finish;
}

template <typename F, typename S>
constexpr auto deep_first(std::pair<F, S> x) {
	return deep_first(x.first);
}

//convenience function for ct_tuple<...>::map.
template <auto callable, typename ...Ts>
constexpr auto map(ct_tuple<Ts...> tpl) {
	return tpl.template map<callable>();
}

// checks wheter a and b have the same type and
// are equal.
template <typename T, typename S>
constexpr bool is_equal(T a, S b) {
	if constexpr (std::is_same_v<std::remove_cv_t<T>, std::remove_cv_t<S>>)
		return a == b;
	else return false;
}

//checks wheter an element val is contained at some point in a
//tuple tpl
template <typename T, typename ...Ts>
constexpr bool contains(T val, ct_tuple<Ts...> tpl) {
	bool this_lvl_eq = is_equal(tpl.fst, val);
	if constexpr (tpl.size() == 1)
		return this_lvl_eq;
	else
		return this_lvl_eq || contains(val, tpl.rest);
}

template <typename T>
constexpr bool contains(T, empty_tuple) {
	return false;
}

//removes all occurences of val in tpl.
template <auto val, auto tpl>
constexpr auto remove()
{
	constexpr bool this_lvl_eq = is_equal<decltype(tpl.fst), decltype(val)>(tpl.fst, val);
	if constexpr (tpl.size() == 1)
		if constexpr (this_lvl_eq) return empty_tuple{};
		else return tpl;
	else
		if constexpr (this_lvl_eq)return  remove<val, tpl.rest>();
		else return ct_tuple{ tpl.fst } + remove<val, tpl.rest>();
}

//removes all elements of a tuple for which predicate(elem) == false.
template <auto predicate, auto tpl> requires (!is_compound_tuple<decltype(tpl)>)
constexpr auto filter() {
	if constexpr (predicate(tpl.fst)) return ct_tuple{ tpl.fst };
	else return empty_tuple{};
}

template <auto predicate, auto tpl> requires (is_compound_tuple<decltype(tpl)>)
constexpr auto filter() {
	if constexpr (predicate(tpl.fst)) return ct_tuple{ tpl.fst } + filter<predicate, tpl.rest>();
	else return filter<predicate, tpl.rest>();
}

//returns the first occurence of the val in tpl
template<auto callable, typename S, typename T>
constexpr S find_first(ct_tuple<T> tpl, S val) {
	if constexpr (!std::is_same_v<T, S>) return S{};
	else
		if (callable(tpl.fst, val)) return tpl.fst;
		else return {};
}

template<auto callable, typename S, typename T, typename ...Ts> requires (sizeof...(Ts) > 0)
constexpr S find_first(ct_tuple<T, Ts...> tpl, S val) {
	if constexpr (!std::is_same_v<T, S>) return find_first<callable>(tpl.rest, val);
	else
		if (callable(tpl.fst, val)) return tpl.fst;
		else return find_first<callable>(tpl.rest, val);
}


template <int i, typename ...Ts> requires (i > 0)
constexpr auto get_helper(ct_tuple<Ts...> c) {
	static_assert(i >= 0);
	if constexpr (i > 0) {
		return get_helper<i - 1>(c).rest;
	}
}

template <int i, typename ...Ts> requires (i == 0)
constexpr auto get_helper(ct_tuple<Ts...> c) {
	static_assert(i >= 0);
	return c;
}

//get the i-th element of a tuple c
template <int i, typename ...Ts>
constexpr auto get(ct_tuple<Ts...> c) {
	static_assert(i >= 0);
	return get_helper<i>(c).fst;
}

template<typename T, typename ... Ts>
consteval auto push_front(ct_tuple<Ts...> arr, const T& elem) {
	return ct_tuple<T, Ts...>{elem, arr};
}



// Parser Types: types to simplify expression
// of productions
// example:
// instead of 
// S -> S t
// S -> epsilon
// you can use the Many type:
// S -> Many{t}
// which is equivalent to the above two 
// producitons.
// ----------------------------------------
// 

template <typename...Ts> //requires  (is_parser_type<Ts>() &&...)
struct Seq {
	constexpr Seq(Ts... args) : rep{ args... } {}
	constexpr Seq(ct_tuple< Ts...> rep) : rep{ rep } {}
	ct_tuple< Ts...> rep;

	constexpr bool simple() const { return 1 == sizeof...(Ts); }
	constexpr auto get_rest() {
		static_assert(!simple());
		return rep.rest;
	}

	constexpr bool operator==(const Seq& r) const {
		return rep == r.rep;
	}


};

template <typename...Ts> //requires  (is_parser_type<Ts>() &&...)
struct Alt {
	constexpr Alt(Ts... args) : rep{ args... } {}
	constexpr Alt(ct_tuple< Ts...> rep) : rep{ rep } {}
	ct_tuple< Ts...> rep;

	static consteval bool simple() { return 1 == sizeof...(Ts); } 

	constexpr auto get_rest() {
		static_assert(!simple());
		return rep.rest;
	}

	constexpr bool operator==(const Alt& r) const {
		return rep == r.rep;
	}


};


template <typename T> //requires (is_parser_type<T>())
struct Many {
	constexpr Many(const T& args) : many{ args } {}
	T many;


};


// represents a production rule in a grammar:
// example:
// S -> a
//==> Rule(S,a)
template <typename T, typename RuleType> 
struct Rule {
	constexpr Rule(RuleType lhs, T rhs) : lhs{ lhs }, rhs{ rhs }{}
	RuleType lhs;
	T rhs;
	constexpr bool operator== (const Rule& r)  const
	{
		return lhs == r.lhs && rhs == r.rhs;
	}
};


template <typename Kind,typename NonterminalKind>
struct ParseError {
	size_t Where;	//where the error was found.
	Kind expected;	//what terminal type was expected
	Kind there;		//what was actually found
	std::string message;
};

//helper concept, see below.
template <typename T>
concept has_children = requires{ T::children; };

template <typename Kind, typename NonterminalKind>
struct ct {
	
	//simple "pair" of <type,lexme>. the lenght is included here to represent the
	//length of the token as a string.
	struct Token {
		Kind kind;
		int length;
		std::string content;
	};

	//Parser related "concepts"
	//-------------------------

	template <typename T>
	static consteval bool is_terminal() {  return std::is_same_v<T, Kind> || std::is_same_v<T, epsilon>; };
	template <typename T>
	static consteval bool is_nonterminal() { return std::is_same_v<T, NonterminalKind> || is_pair<T>; }

	template <typename T>
	static consteval bool is_anyterminal() {
		return std::is_same_v<T, Kind> || std::is_same_v<T, NonterminalKind>
			|| is_pair<T> || std::is_same_v<T, epsilon>;
	}

	template <typename T>
	static consteval bool is_parser_type() {
		return is_seq<T> || is_many<T> || is_anyterminal<T>() || std::is_same_v<T, epsilon>;
	}
	
	template <typename T>
	static consteval bool is_parse_tree() {
		return std::is_constructible_v<T, Token, std::vector<T>> 
			&& std::is_constructible_v<T, NonterminalKind, std::vector<T>>
			&& has_children<T>;
	}

	//Rule transformer
	//----------------


	struct transformer_helper { //static helper class to enable circular dependencies.


		template<int i = 0, typename RuleType, typename ...Ts>
		static consteval auto transform_rule(const Rule<Seq<Ts...>, RuleType> rule) {
			//decide wheter we need to emit a new rule:
			if constexpr (!is_anyterminal<decltype(get<i>(rule.rhs.rep))>()) {
				//update the rule: replace the special part of the Seq with a new nonterminal (lhs,i)
				auto new_rhs = Seq{ rule.rhs.rep.template replace<i>(std::pair{ rule.lhs,i }) };
				auto new_Rule = Rule{
					rule.lhs,
					new_rhs
				};
				auto processed_rule = transform_rule(
					Rule{ std::make_pair(rule.lhs,i), get<i>(rule.rhs.rep) }
				);
				if constexpr (sizeof...(Ts) - 1 <= i)
					//"break" last part of Seq processed, (was special)
					return processed_rule + ct_tuple{ new_Rule };
				else
					//process next, (was special)
					return processed_rule + transform_rule<i + 1>(new_Rule);
			}
			//no new rule necessary
			else if constexpr (sizeof...(Ts) - 1 > i)
				return transform_rule<i + 1>(rule); //process next; (was anyterminal)
			else return ct_tuple{ rule }; //"break" last part of Seq processed, was anyterminal
		}

		template<int i = 0, typename N, typename ...Ts>
		static consteval auto transform_rule(const Rule<Many<Ts...>, N> rule) {
			//emit a new epsilon production for termination and a right recursive "many" production
			return transform_rule(Rule{ rule.lhs, Seq{rule.rhs.many,rule.lhs} })
				+ ct_tuple{ Rule{rule.lhs, epsilon{} } };
		}
		/*ret type ct_tuple<Rule<Ts,N>...> wrong, because may generate additional rules*/
		template<typename N, typename ...Ts>
		static consteval auto  alt_helper(N lhs, Alt<Ts...> rule) {
			//iterate through all parts of the Alt and emit for each a new production rule:
			if constexpr (sizeof...(Ts) > 1)
				return  transform_rule(Rule{ lhs, get<0>(rule.rep) }) +
				alt_helper(lhs, Alt{ rule.get_rest() });
			else return transform_rule(Rule{ lhs, get<0>(rule.rep) });
		}

		template<int i = 0, typename RuleType, typename ...Ts>
		static consteval auto transform_rule(Rule<Alt<Ts...>, RuleType> rule) {
			return alt_helper(rule.lhs, rule.rhs);
		}

		//if its a anyterminal, just do nothing:
		template<typename T, typename N>
		requires (is_anyterminal<T>())
			static consteval auto transform_rule(Rule<T, N> rhs) { return ct_tuple{ rhs }; }


	};

	//function for easier access
	template<typename T>
	static consteval auto transform_rule_(T rule) {
		return transformer_helper::transform_rule(rule);
	}

	template<typename... Ts, int ...Is >
	static consteval auto transform_grammar_impl(ct_tuple<Ts...> rules, std::integer_sequence<int, Is...> s) {
		return (transform_rule_(get<Is>(rules)) + ...);
	}

	template<typename... Ts>
	static auto consteval transform_grammar(ct_tuple<Ts...> rules) {
		return transform_grammar_impl(rules, std::make_integer_sequence<int, sizeof...(Ts)>{});
	}



	//using namespace ct;

	template <typename T, typename ...Ts>
	static constexpr auto remove_epsilon(ct_tuple<T, Ts...> tpl) {
		if constexpr (std::is_same_v<T, epsilon>) {
			if constexpr (sizeof...(Ts) == 0) return empty_tuple{};
			else return remove_epsilon(tpl.rest);
		}
		else
		{
			if constexpr (sizeof...(Ts) == 0) return ct_tuple{ tpl.fst };
			else return ct_tuple{ tpl.fst } + remove_epsilon(tpl.rest);
		}
	}

	template <auto rules, auto nonterminal >
	static consteval auto find_rule() {
		return filter < [](auto rule) {
			if constexpr (std::is_same_v<std::remove_cv_t<decltype(rule.lhs)>, 
					std::remove_cv_t<decltype(nonterminal)>>) {
				return rule.lhs == nonterminal;
			}
			else {
				return false;
			}
		}, rules > ();
	}
	template <auto rules>
	struct first_helper {
		template <auto terminal>
		requires (is_terminal<decltype(terminal)>())
			static consteval ct_tuple<decltype(terminal)> first() {
			return { terminal };
		}
		template <auto nonterminalSeq> requires is_seq<decltype(nonterminalSeq)>
		static consteval auto first() {
			constexpr auto fst_iteration = first<nonterminalSeq.rep.fst>();

			//in theory, the code could be simplified to if constexpr (is_comp_tpl <...> && contains(...))
			//but strangly, g++ complains if we do that.
			if constexpr (is_compound_tuple<decltype(nonterminalSeq.rep)>) {
				if constexpr (contains(epsilon{}, fst_iteration)) {
					constexpr auto rest = nonterminalSeq.rep.rest;
					return remove_epsilon(fst_iteration) + first < Seq{ rest } > ();
				}
				else return fst_iteration;
			}
			else return fst_iteration;
		}

		template <auto nonterminal>
		static consteval auto first() {
			//find produciton rule
			constexpr auto found_rules = find_rule<rules, nonterminal>();
			using NT = decltype(found_rules);
			if constexpr (std::is_same_v<std::remove_cv_t<NT>, empty_tuple >)
				return ct_tuple{ epsilon{} };
			else
			{
				return collect_fsts<found_rules>();
			}
		}

		template <auto found_rules>
		static constexpr auto collect_fsts() {
			if constexpr (std::is_same_v<std::remove_cv_t<decltype(found_rules)>, 
				empty_tuple>)
				return empty_tuple{};
			else {
				constexpr auto found_rules_front = found_rules.fst;
				if constexpr (is_compound_tuple<decltype(found_rules)>)
					return collect_fsts<found_rules.rest>() + first<found_rules_front.rhs>();
				else return first<found_rules_front.rhs>();
			}
		}

	};

	//generate first set for parse_ty = either a seq or an terminal / nonterminal
	template <auto rules, auto parser_ty>
	static consteval auto first() {
		return first_helper<rules>::template first<parser_ty>();
	}

	
	template <auto found_rules, auto grammar>
	static constexpr auto construct_choice() {
		static_assert(found_rules.size() != 0, "invalid rules");
		constexpr auto rule = found_rules.fst;
		constexpr auto rule_fst_pair = std::make_pair(first<grammar, rule.rhs>(), rule);
		if constexpr (found_rules.size() > 1)
			return ct_tuple{ rule_fst_pair , construct_choice<found_rules.rest,grammar>() };
		else return ct_tuple{ rule_fst_pair };
	}

	
	template <typename ParseTree> requires (is_parse_tree<ParseTree>())
	struct parser_helper {

		template <auto rules, auto choice>
		static ParseTree find_right_rule(Kind lookahead, std::span<Token> input_stream) {
			if constexpr (!is_compound_tuple<decltype(choice)>){//(choice.size() == 1) {
				if (contains(lookahead, choice.fst.first)||contains(epsilon{}, choice.fst.first))
					return ParseTree(
						deep_first(choice.fst.second.lhs),
						(std::vector<ParseTree>)expect<choice.fst.second.rhs, rules>(input_stream)
					); 
				if constexpr (choice.fst.first.size() > 0) {
					if constexpr (std::is_same_v<
						std::remove_const_t<decltype(choice.fst.first.fst)>,Kind>
					)
						throw ParseError<Kind, NonterminalKind>{
							.Where = input_stream.size(),
							.expected = (Kind)choice.fst.first.fst,
							.there = lookahead,
							.message = "no rule for lookahead \"there\" found."
						};
					else
						throw ParseError<Kind, NonterminalKind>{
						.Where = input_stream.size(),
								.there = lookahead,
								.message = "fatal error: tried to apply epsilon rule" + std::string(typeid (choice.fst.first.fst).name())
					};
				}
				throw std::logic_error{ "Panic!, empty tuple as choice given." };
			}
			else {
				if (contains(lookahead, choice.fst.first) || contains(epsilon{}, choice.fst.first))
					return ParseTree(
						deep_first(choice.fst.second.lhs),
						expect<choice.fst.second.rhs, rules>(input_stream)
				);
				else return find_right_rule<rules, choice.rest>(lookahead, input_stream);
			}
			
		}

		template <auto nonterm, auto rules>
		static ParseTree apply_rule(std::span<Token> input_stream) {
			constexpr auto found_rules = find_rule<rules, nonterm>();
			constexpr auto choice = construct_choice<found_rules, rules>();

			Kind lookahead = input_stream.size() ? input_stream.front().kind : Kind::end_of_file;
			return find_right_rule<rules, choice>(lookahead, input_stream);
		}

		template <auto rhs, auto rules> requires (is_nonterminal<decltype(rhs)>())
		static std::vector<ParseTree> expect(std::span<Token> input_stream) {
			if constexpr (is_pair<decltype(rhs)>)
				return apply_rule<rhs, rules>(input_stream).children;
			else
				return std::vector<ParseTree>{ apply_rule<rhs,rules>(input_stream) };
		}

		template <auto rhs, auto rules> requires (is_terminal<std::remove_cv_t<decltype(rhs)>>())
		static  std::vector<ParseTree> expect(std::span<Token> input_stream) {
			if (input_stream.empty()) {
				if constexpr (std::is_same_v<std::remove_cv_t<decltype(rhs)>, epsilon>)
					return {};
				else
					throw ParseError<Kind, NonterminalKind>{
						.Where = input_stream.size(),
						.expected = rhs,
						.message = "unexpected end of input."
					};
			}

			using namespace std::string_literals;
			//check for correct kind:
			auto to_check = input_stream.front();
			if (!is_equal(to_check.kind, rhs))
			{
				if constexpr (std::is_same_v<std::remove_cv_t<decltype(rhs)>, epsilon>)
					return {};
				else
					throw ParseError<Kind, NonterminalKind>{
						.Where = input_stream.size(), 
						.expected = rhs, 
						.there = to_check.kind,
						.message = "unexpected token"};
			}

			return { ParseTree{ to_check } };
		}

		template <auto rhs, auto rules> requires is_seq<decltype(rhs)>
		static std::vector<ParseTree> expect(std::span<Token> input_stream) {

			
			std::vector<ParseTree> res = expect<rhs.rep.fst, rules>(input_stream);
			int length = std::accumulate(res.cbegin(), res.cend(), 0,
				[](auto a, auto b) {return a + b.getLength(); });
			if constexpr (!rhs.simple()) {
				auto rest = expect < Seq{ rhs.rep.rest }, rules > (input_stream.subspan(length));
				res.insert(res.end(), rest.begin(), rest.end());
			}
			return res;

		}
	};

};

template <typename Kind, typename NonterminalKind, typename ParseTree, auto rules, NonterminalKind startsymbol>
ParseTree parse(std::span<typename ct<Kind,NonterminalKind>::Token> input_stream) {

	ParseTree pt = ct<Kind, NonterminalKind>::template parser_helper<ParseTree>::template expect<startsymbol, rules>(input_stream)[0];
	if (pt.getLength() != input_stream.size()) 
		throw ParseError<Kind, NonterminalKind>{ 
			.Where = (size_t)pt.getLength(), 
			.there = input_stream[pt.getLength()].kind,
			.message = "unexpected end of input" };
	return pt;
}

}
#endif