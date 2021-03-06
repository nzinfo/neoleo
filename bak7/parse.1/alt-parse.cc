/*
 * This file is part of Oleo, the GNU Spreadsheet.
 * 
 * Oleo is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * Oleo is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Oleo; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <algorithm>
#include <cassert>
#include <cctype>
#include <cmath>
#include <cstdlib>
#include <exception>
#include <functional>
#include <iostream>
#include <map>
#include <memory>
#include <stdexcept>
#include <regex>
#include <string>
#include <type_traits>
#include <typeinfo>
#include <variant>
#include <vector>

#include "alt-cells.h"
#include "alt-lex.h"
#include "numeric.h"

using std::cout;
using std::endl;
using std::map;
using std::make_shared;
using std::make_unique;
using std::shared_ptr;
using std::string;
using std::unique_ptr;
using std::variant;
using std::vector;
using namespace std::string_literals;

typedef std::vector<std::string> strings;
typedef map<string, value_t> varmap_t;



///////////////////////////////////////////////////////////////////////////
// scanner

// https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#classic
// [ ] enclose an optional part of the expression
// { } enclose 0 or more repetitions
// | separates alternatives
// Parse rules (incomplete):
//  E --> R {( "<" | "<=" | ">" | ">=" | "==" | "!=" ) R}
//  R --> T {( "+" | "-" ) T}
//  T --> P {( "*" | "/" ) P}
//  P --> v | F | "(" E ")" | "-" T
//  F --> word "(" [E {"," E} ] ")"






using math_op = std::function<value_t(value_t, value_t)>;

value_t neo_add(value_t v1, value_t v2) { return num(v1) + num(v2); }
value_t neo_sub(value_t v1, value_t v2) { return num(v1) - num(v2); }
value_t neo_mul(value_t v1, value_t v2) { return num(v1) * num(v2); }
value_t neo_div(value_t v1, value_t v2) { return num(v1) / num(v2); }
value_t neo_lt(value_t v1, value_t v2) { return num(v1) < num(v2); }
value_t neo_le(value_t v1, value_t v2) { return num(v1) <= num(v2); }
value_t neo_gt(value_t v1, value_t v2) { return num(v1) > num(v2); }
value_t neo_ge(value_t v1, value_t v2) { return num(v1) >= num(v2); }
value_t neo_eq(value_t v1, value_t v2) { return num(v1) == num(v2); }
value_t neo_ne(value_t v1, value_t v2) { return num(v1) != num(v2); }

map<std::string, math_op> math_ops = { 
	{ "+", neo_add} , {"-", neo_sub }, 
	{"*",  neo_mul}, {"/", neo_div}, 
	{"<", neo_lt}, {"<=", neo_le}, {">", neo_gt}, {">=", neo_ge}, {"=", neo_eq}, {"!=", neo_ne}
};

using neo_func = std::function<value_t(values vs)>;

value_t neo_println(values vs)
{
	cout << "neo_println() says hello\n";
	cout << "number of args: " << vs.size() << "\n";
	for(auto& v: vs)
		cout << num(v)  << ", ";
	cout << endl;
	return 0;
}

value_t neo_mod(values vs)
{
	if(vs.size() != 2) throw std::runtime_error("mod nargs");
	return std::fmod(num(vs[0]), num(vs[1]));
}

value_t neo_pi(values vs)
{
	if(vs.size() != 0) throw std::runtime_error("pi nargs");
	return 3.141592653589793238;
}

value_t neo_sqrt(values vs)
{
	if(vs.size() != 1) throw std::runtime_error("sqrt nargs");
	return std::sqrt(num(vs[0]));
}

map<std::string, neo_func> neo_funcs = {
	{"cell", neo_cell},
	{"mod", neo_mod},
	{"pi", neo_pi},
	{"println", neo_println},
	{"sqrt", neo_sqrt}
};


class Factor;
//class Relop;
//class Term;


/*
class Expression {
	public:
		vector<Relop> operands; // terms;
		vector<math_op> fops;
		//const strings operators = strings {"+", "-" };
};
*/


class Expression {
	public:
		vector<Factor> operands; // terms;
		vector<math_op> fops;
};

value_t eval(varmap_t& vars, Expression e);

/*
class Relop {
	public:
		vector<Term> operands;
		vector<math_op> fops;
};

class Term {
	public:
		vector<Factor> operands;
		vector<math_op> fops;
		//const strings operators = strings { "*", "/" };
};
*/

class FuncCall{
	public:
		string name; // TODO evaluation would be quicker with a function pointer.
		vector<Expression> exprs;
};

class Variable {
	public:
		string name;
};

class Factor {
	public:
		variant<num_t, Expression, FuncCall, Variable> factor;
};

class FuncDef{
	public:
		string name;
		strings args;
		vector<Expression> statements; // TODO expressions are not statements
};





typedef lexemes::iterator lex_it;


typedef vector<vector<string>> prec_t;
typedef prec_t::iterator prit_t;
static auto precedence_chain = prec_t { {"<=", "<", ">=", ">", "=", "!="}, {"+", "-"}, {"*", "/"}};

Expression make_expression(lexemes_c& tokes, prec_t chain = precedence_chain);


FuncCall
make_funccall(lexemes_c& tokes)
{
	FuncCall fn;
	fn.name = tokes.curr();
	tokes.advance();
	tokes.require("(");
	tokes.advance();
	auto make = [&]() { fn.exprs.push_back(make_expression(tokes)); };
	if(tokes.curr() != ")") {
		make();
		while(tokes.curr() == ",") {
			tokes.advance();
			make();
		}
	}
	tokes.advance();
	return fn;
}

Variable
make_variable(lexemes_c& tokes)
{
	Variable var;
	var.name = tokes.curr();
	cout << "make_variable():variable:" << var.name << "\n";
	tokes.advance();
	return var;
}

Factor
make_factor(lexemes_c& tokes)
{
	Factor f;
	if(tokes.curr() == "(") { // a parenthesised expression
		tokes.advance();
		f.factor = make_expression(tokes);
		tokes.require(")");
		tokes.advance();
	} else if(tokes.curr_type() == LT_ID) {
		f.factor = make_funccall(tokes);
	} else if(tokes.curr_type() == LT_VAR) {
		f.factor = make_variable(tokes);
	} else {
		num_t v = std::stod(tokes.curr());
		f.factor = v;
		tokes.advance();
	}

	return f;
}

/*
Term
make_term(lexemes_c& tokes)
{
	Term  t;
	auto operators = strings {"*", "/"};
	auto make = make_factor;
	//cout << "decltype(make_factor):" << type_name<decltype(make)>() << "\n";
	t.operands.push_back(make(tokes));
	while(1) {
		std::string op = tokes.curr();
		strings::iterator opit = std::find(operators.begin(), operators.end(), op);
		if(opit == operators.end()) break;
		tokes.advance();
		math_op fop = (math_ops.find(op)->second);
		t.fops.push_back(fop);
		t.operands.push_back(make(tokes));
	}
	return t;
}

Relop
make_relop(lexemes_c& tokes)
{
	Relop r;
	auto make = make_term;
	auto operators = strings {"<", "<=", ">", ">=", "=", "!"};
	r.operands.push_back(make(tokes));
	while(1) {
		std::string op = tokes.curr();
		strings::iterator opit = std::find(operators.begin(), operators.end(), op);
		if(opit == operators.end()) break;
		tokes.advance();
		math_op fop = (math_ops.find(op)->second);
		r.fops.push_back(fop);
		r.operands.push_back(make(tokes));
	}
	return r;
}
*/

Expression
make_expression(lexemes_c& tokes, prit_t prit)
{
	Expression e;
	return e;

	/*
	auto make = make_relop;
	auto operators = strings {"+", "-"};
	e.operands.push_back(make(tokes));
	while(1) {
		std::string op = tokes.curr();
		strings::iterator opit = std::find(operators.begin(), operators.end(), op);
		if(opit == operators.end()) break;
		tokes.advance();
		math_op fop = (math_ops.find(op)->second);
		e.fops.push_back(fop);
		e.operands.push_back(make(tokes));
	}
	return e;
	*/
}

Expression
parse_expression(std::string s)
{
	//cout << "parsing: " << s << endl;
	lexemes_c tokes{alt_yylex_a(s)};
	return make_expression(tokes);
}


// Used to turn a user-defined function into a neo_func
value_t wrap_defun(FuncDef func, values vs)
{
	// bind argument identifiers to values
	if(vs.size() != func.args.size()) 
		throw std::runtime_error("#FUNC_ARGS:" + func.name 
				+ "():Expected " + std::to_string(func.args.size()) 
				+ " args, got " + std::to_string(vs.size()));
	varmap_t vars;
	for(int i=0; i< vs.size(); ++i)
		vars[func.args[i]] = vs[i];

	//cout << "wrap_defun():arg[0]:" << num(vs[0]) << "\n";

	for(const auto& stm: func.statements)
		eval(vars, stm);
	return 666;
}

bool
define_function(lexemes_c& tokes)
{
	if(tokes.curr() != "FUNC") return false;
	tokes.advance();
	FuncDef func;

	func.name = tokes.curr();
	tokes.advance();

	// argument list
	auto append_arg = [&]() { 
		if(tokes.curr_type() != LT_VAR)
			throw std::runtime_error("#PARSE:Function definition of " + func.name 
					+ " has non-var arg " + tokes.curr());
		func.args.push_back(tokes.curr()); 
		tokes.advance(); 
	};
	tokes.require("(");
	tokes.advance();
	if(tokes.curr() != ")") {
		append_arg();
		while(tokes.curr() == ",") {
			tokes.advance();
			append_arg();
		}
	}
	//tokes.require(")");
	tokes.advance();

	// compound statement
	tokes.require("{");
	tokes.advance();
	while(tokes.curr() != "}") {
		func.statements.push_back(make_expression(tokes)); // TODO make_statement()
		//tokes.require(";");
		//tokes.advance();
	}
	tokes.require("}");
	tokes.advance();

	cout << "define_function():name:" << func.name << "\n";
	using namespace std::placeholders;
	neo_funcs[func.name] = std::bind(wrap_defun, func, _1);

	cout << "define_function() returning\n";
	return true;
}


void
parse_program(varmap_t& vars, std::string s)
{
	lexemes_c tokes{alt_yylex_a(s)};
	//base_ptr ast;
	while( tokes.curr_type()!= LT_EOF) {
		if(! define_function(tokes)) {
			cout << "parse_program():curr toke:" << tokes.curr() << "\n";
			Expression e{make_expression(tokes)};
			cout << "Evaluating\n";
			eval(vars, e);
		}
	}
	//return nullptr;
}


///////////////////////////////////////////////////////////////////////////
// eval


	
varmap_t global_varmap = { {"?pi", 3.141592653589793238} };

value_t eval(varmap_t& vars, Variable var)
{
	if(true) {
		cout << "eval(Variable):name:" << var.name <<"\n";
		cout << "Keys are:";
		for(auto it = vars.begin(); it!= vars.end(); it++) cout << it->first << " ";
		cout << endl;
	}

	auto it = vars.find(var.name);
	if(it == vars.end())
		return 0; // TODO should return Empty
	return it->second;
}

value_t eval(varmap_t& vars, FuncCall fn)
{	
	values vs;
	for(const auto& e: fn.exprs)
		vs.push_back(eval(vars, e));	

	auto it  =  neo_funcs.find(fn.name);
	if(it == neo_funcs.end()) {
		throw std::runtime_error("#UNK_FUNC: " + fn.name);
	}
	neo_func f = it->second;
	return f(vs);
}

value_t
eval(varmap_t& vars, Factor f)
{
	if(std::holds_alternative<num_t>(f.factor)) {
		return std::get<num_t>(f.factor);
	} else if(std::holds_alternative<Expression>(f.factor)) {
		return eval(vars, std::get<Expression>(f.factor));
	} else if(std::holds_alternative<FuncCall>(f.factor)) {
		return eval(vars, std::get<FuncCall>(f.factor));
	} else if(std::holds_alternative<Variable>(f.factor)) {
		return eval(vars, std::get<Variable>(f.factor));
	} else {
		throw std::logic_error("eval(Factor f) unhandled alternative");
	}

}



template<class T>
value_t eval_multiop(varmap_t& vars, T expr)
{
	assert(expr.operands.size()>0);
	value_t result = eval(vars, expr.operands[0]);
	for(int i = 0; i< expr.fops.size() ; i++) {
		auto& fop = expr.fops[i];
		result = fop(result, eval(vars, expr.operands[i+1]));
	}
	return result;
}

//value_t eval(varmap_t& vars, Term t) { return eval_multiop(vars, t); }
//value_t eval(varmap_t& vars, Relop r) { return eval_multiop(vars, r); }
value_t eval(varmap_t& vars, Expression e) { return eval_multiop(vars, e); }

string str(value_t v)
{
	return std::to_string(std::get<num_t>(v));
}
void
test_parse_expression(varmap_t vars, std::string s)
{
	cout << "test_parse_expression():" << s << "\n";
	Expression e{parse_expression(s)};
	cout << "Done parsing. Now evaluating." << endl;
	cout << str(eval(vars, e)) << "\n" << endl;
}



///////////////////////////////////////////////////////////////////////////
// tests

static
bool test01()
{
	cout << "test01\n";

	lex_and_print("  r1C2 12.3e23 13.4");
	lex_and_print("goo(bar)");
	lex_and_print("1=2");
	lex_and_print("1!=2");
	lex_and_print("1<2");
	lex_and_print("1<=2");
	lex_and_print("1+2-3");
	lex_and_print("1*3/3");
	lex_and_print("?foo+12");


	return true;
}

static void
test02()
{
	cout << "test02\n";
	auto test_expressions = { "66", "1-2-3", "2*3", "23<24",  "23+24", "1+2+3", "1+2*3", "4*2+3", "4/5/6", 
		"(1+2)*3",  "sqrt(3+9)", "pi()", "mod(12, 7)"};

	for(const string& s: test_expressions)
		test_parse_expression(global_varmap, s);


	//cout << "Now I am going to deliberately throw an error\n";
	//throw syntax_error();
}

void
test03()
{
	cout << "test03\n";
	//std::string prog = "FUNC foo (v) { println(v+1); } func bar () { } println(1, 2, 3, 12+13) foo(5)";
	std::string prog = "FUNC foo (?v) { println(1+1) println(22, 23) println(?v+1) } FUNC bar(?x) { foo(?x*?x) } bar(1+2) println(?pi)";
	//std::string prog = "FUNC foo (v) { println(1+1); }";
	cout << "parsing: " << prog << "\n";
	parse_program(global_varmap, prog);
}


bool run_alt_parse_tests()
{
	test01();
	test02();
	test03();
	return false;
}
