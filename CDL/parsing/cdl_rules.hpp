
// Copyright (c) 2020 Axel Boldt-Christmas
//

#pragma once
#include <optional>
#include <string>
#include <utility>
#include <variant>

#include "base_rules.hpp"

namespace parsing::rules {
using parsing::rules::Tokenize;
using parsing::rules::non_terminal::And;
using parsing::rules::non_terminal::Not;
using parsing::rules::non_terminal::Opt;
using parsing::rules::non_terminal::Or;
using parsing::rules::non_terminal::Plus;
using parsing::rules::non_terminal::Seq;
using parsing::rules::non_terminal::Star;
using parsing::rules::terminal::AnyLiteral;
using parsing::rules::terminal::Classification;
using parsing::rules::terminal::ClassLiteral;
using parsing::rules::terminal::Literal;
using parsing::rules::terminal::RangeLiteral;
using parsing::rules::terminal::String;
// forward declarations
struct Form;
struct BindingForm;

// literal tokens
struct DefLiteral : PARSING_RULE_TERMINAL_STRING("def", Tokenize::Yes) {};
struct IfLiteral : PARSING_RULE_TERMINAL_STRING("if", Tokenize::Yes) {};
struct DoLiteral : PARSING_RULE_TERMINAL_STRING("do", Tokenize::Yes) {};
struct LetLiteral : PARSING_RULE_TERMINAL_STRING("let", Tokenize::Yes) {};
struct QuoteLiteral : PARSING_RULE_TERMINAL_STRING("quote", Tokenize::Yes) {};
struct VarLiteral : PARSING_RULE_TERMINAL_STRING("var", Tokenize::Yes) {};
struct FnLiteral : PARSING_RULE_TERMINAL_STRING("fn", Tokenize::Yes) {};
struct LoopLiteral : PARSING_RULE_TERMINAL_STRING("loop", Tokenize::Yes) {};
struct RecurLiteral : PARSING_RULE_TERMINAL_STRING("recur", Tokenize::Yes) {};
struct ThrowLiteral : PARSING_RULE_TERMINAL_STRING("throw", Tokenize::Yes) {};
struct TryLiteral : PARSING_RULE_TERMINAL_STRING("try", Tokenize::Yes) {};

struct NilLiteral : PARSING_RULE_TERMINAL_STRING("nil", Tokenize::Yes) {};
struct TrueLiteral : PARSING_RULE_TERMINAL_STRING("true", Tokenize::Yes) {};
struct FalseLiteral : PARSING_RULE_TERMINAL_STRING("false", Tokenize::Yes) {};

struct WhitespaceLiteral
    : Or<ClassLiteral<Classification::Whitespace>, Literal<U','>> {};

struct SymbolLiteral
    : Or<ClassLiteral<Classification::Alphanumeric, Tokenize::Yes>,
         Literal<U'*', Tokenize::Yes>, Literal<U'+', Tokenize::Yes>,
         Literal<U'!', Tokenize::Yes>, Literal<U'-', Tokenize::Yes>,
         Literal<U'_', Tokenize::Yes>, Literal<U'\'', Tokenize::Yes>,
         Literal<U'?', Tokenize::Yes>, Literal<U'<', Tokenize::Yes>,
         Literal<U'>', Tokenize::Yes>, Literal<U'=', Tokenize::Yes>> {};
struct SymbolFirstLiteral
    : Or<ClassLiteral<Classification::Alphabetic, Tokenize::Yes>,
         Literal<U'*', Tokenize::Yes>, Literal<U'+', Tokenize::Yes>,
         Literal<U'!', Tokenize::Yes>, Literal<U'-', Tokenize::Yes>,
         Literal<U'_', Tokenize::Yes>, Literal<U'\'', Tokenize::Yes>,
         Literal<U'?', Tokenize::Yes>, Literal<U'<', Tokenize::Yes>,
         Literal<U'>', Tokenize::Yes>, Literal<U'=', Tokenize::Yes>> {};

// terminal tokens
struct AllWhitespace : Star<WhitespaceLiteral> {};
struct Boolean : Or<TrueLiteral, FalseLiteral> {};
struct Nil : NilLiteral {};

struct Symbol : Seq<AllWhitespace, Not<Or<Boolean, Nil>>, SymbolFirstLiteral,
                    Star<SymbolLiteral>, AllWhitespace> {};

struct Number : Plus<ClassLiteral<Classification::Digit>> {};
struct Keyword : Seq<Literal<U':'>, Opt<Literal<U':', Tokenize::Yes>>, Symbol> {
};
struct Character : Seq<Literal<U'\\'>, Plus<AnyLiteral<Tokenize::Yes>>> {};
struct QuoteString
    : Seq<Literal<U'"'>,
          Star<Or<Seq<Literal<U'\\'>, AnyLiteral<Tokenize::Yes>>,
                  Seq<Not<Literal<U'"'>>, AnyLiteral<Tokenize::Yes>>>>,
          Literal<U'"'>> {};
struct Value : Seq<AllWhitespace,
                   Or<Number, QuoteString, Character, Nil, Boolean, Keyword>,
                   AllWhitespace> {};

// non-terminal tokens
struct DefForm : Seq<DefLiteral, Symbol, Opt<Form>> {};
struct IfForm : Seq<IfLiteral, Form, Form, Opt<Form>> {};
struct DoForm : Seq<DoLiteral, Star<Form>> {};
struct LetForm : Seq<LetLiteral, AllWhitespace, Literal<U'['>,
                     Star<BindingForm, Form>, Literal<U']'>, Star<Form>> {};
struct QuoteForm : Seq<QuoteLiteral, Form> {};
struct VarForm : Seq<VarLiteral, Form> {};
struct FnBody : Seq<Literal<U'['>, Star<BindingForm>, AllWhitespace,
                    Opt<Literal<U'&'>, Symbol>, Literal<U']'>, Star<Form>> {};
struct MultiFnBody : Plus<Literal<U'('>, AllWhitespace, FnBody, Literal<U')'>> {
};
struct FnForm
    : Seq<FnLiteral, WhitespaceLiteral, Opt<Symbol>, Or<FnBody, MultiFnBody>> {
};
struct LoopForm : Seq<LoopLiteral, AllWhitespace, Literal<U'['>,
                      Star<BindingForm, Form>, Literal<U']'>, Star<Form>> {};
struct RecurForm : Seq<RecurLiteral, Star<Form>> {};
struct ThrowForm : Seq<ThrowLiteral, Form> {};
struct TryForm : Seq<TryLiteral, Seq<>> {};

struct SpecialForm
    : Seq<Literal<U'('>, AllWhitespace,
          Or<DefForm, IfForm, DoForm, LetForm, QuoteForm, VarForm, FnForm,
             LoopForm, RecurForm, ThrowForm, TryForm>,
          AllWhitespace, Literal<U')'>> {};
struct List : Seq<Literal<U'('>, Star<Form>, Literal<U')'>> {};
struct Vector : Seq<Literal<U'['>, Star<Form>, Literal<U']'>> {};
struct Map : Seq<Literal<U'{'>, Star<Form, Form>, Literal<U'}'>> {};
struct Set : Seq<Literal<U'#'>, Literal<U'{'>, Star<Form>, Literal<U'}'>> {};

struct SymbolVector : Seq<AllWhitespace, Literal<U'['>, Star<Symbol>,
                          Literal<U']'>, AllWhitespace> {};
struct BindingVector
    : Seq<Literal<U'['>, Star<BindingForm>, Opt<Literal<U'&'>, Symbol>,
          Opt<PARSING_RULE_TERMINAL_STRING(":as"), Symbol>, Literal<U']'>> {};
struct BindingMap
    : Seq<Literal<U'{'>,
          Opt<PARSING_RULE_TERMINAL_STRING(":keys"), SymbolVector>,
          Star<BindingForm, Form>,
          Opt<PARSING_RULE_TERMINAL_STRING(":as"), Symbol>,
          Opt<PARSING_RULE_TERMINAL_STRING(":or"), Map>, Literal<U'}'>> {};

struct BindingForm
    : Seq<AllWhitespace, Or<Symbol, BindingVector, BindingMap>, AllWhitespace> {
};  // namespace parsing::rules

struct Form
    : Seq<AllWhitespace, Or<Symbol, Value, SpecialForm, List, Vector, Map, Set>,
          AllWhitespace> {};
struct Program : Star<Form> {};

}  // namespace parsing::rules