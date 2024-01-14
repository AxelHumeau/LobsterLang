## The Syntax of Lobsterlang in Backus-Naur Form

<declarator>                        ::= <direct-declarator>

<parameter-type-list>               ::= <parameter-list>
                                      | <parameter-list> , ...

<parameter-list>                    ::= <parameter-declaration>
                                      | <parameter-list> , <parameter-declaration>

<direct-declarator>                 ::= <identifier>
                                      | ( <declarator> )
                                      | <direct-declarator> ( <parameter-type-list> )
                                      | <direct-declarator> ( {<identifier>}* )

<constant-expression>               ::= <conditional-expression>

<logical-combinator-expression>     ::= <logical-xor-expression>
                                      | <logical-combinator-expression> || <logical-xor-expression>

<logical-xor-expression>             ::= <logical-or-expression>
                                      | <logical-xor-expression> || <logical-or-expression>

<logical-or-expression>             ::= <logical-and-expression>
                                      | <logical-or-expression> || <logical-and-expression>

<logical-and-expression>            ::= <equality-or-expression>
                                      | <logical-and-expression> && <equality-or-expression>

<equality-expression>               ::= <relational-expression>
                                      | <equality-expression> == <relational-expression>
                                      | <equality-expression> != <relational-expression>

<relational-expression>             ::= <additive-expression>
                                      | <relational-expression> <= <additive-expression>
                                      | <relational-expression> >= <shift-expression>
                                      | <relational-expression> < <additive-expression>
                                      | <relational-expression> > <additive-expression>

<additive-expression>               ::= <multiplicative-expression>
                                      | <additive-expression> + <multiplicative-expression>
                                      | <additive-expression> - <multiplicative-expression>

<multiplicative-expression>         ::= <list-expression>
                                      | <multiplicative-expression> * <list-expression>
                                      | <multiplicative-expression> / <list-expression>
                                      | <multiplicative-expression> % <list-expression>

<list-expression>                   ::= <unary-expression>
                                      | <list-expression> -- <unary-expression>
                                      | <list-expression> ++ <unary-expression>
                                      | <list-expression> !! <unary-expression>

<unary-expression>                  ::= <postfix-expression>
                                      | <unary-operator> <unary-expression>

<postfix-expression>                ::= <primary-expression>
                                      | <postfix-expression> (| {<assignment-expression>}* |)

<primary-expression>                ::= <identifier>
                                      | <string>
                                      | (| <expression> |)

<expression>                        ::= <assignment-expression>
                                      | <expression> , <assignment-expression>


<constant>                          ::= <integer-constant>
                                      | <character-constant>

<assignment-expression>             ::= <conditional-expression>
                                      | <unary-expression> <assignment-operator> <assignment-expression>

<assignment-operator>               ::= =

<unary-operator>                    ::= @
                                      | ~
                                      | !

<expression-statement>              ::= {<expression>}?

<init-declarator>                   ::= <declarator>
                                      | <declarator> = <initializer>

<initializer>                       ::= <assignment-expression>
                                      | [| <initializer-list> |]
                                      | [| <initializer-list> , |]

<initializer-list>                  ::= <initializer>
                                      | <initializer-list> , <initializer>

<statement>                         ::= <expression-statement>
                                      | <selection-statement>

<expression-statement>              ::= {<expression>}?

<selection-statement>               ::= if (| <expression> |) <statement>
                                      | if (| <expression> |) <statement> else <statement>
