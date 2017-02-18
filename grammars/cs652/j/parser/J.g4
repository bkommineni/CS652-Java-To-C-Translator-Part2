grammar J;

file:  classDeclaration* main EOF
    ;

main:  (fieldDeclaration | statement)*
    ;

classDeclaration
    :   'class' Identifier ('extends' typeType)? classBody ;

classBody
    :   '{' classBodyDeclaration* '}'
    ;

classBodyDeclaration
    :   ';'
    |   block
    |   memberDeclaration
    ;

memberDeclaration
    :   methodDeclaration
    |   fieldDeclaration
    ;

methodDeclaration
    :   (typeType|'void') Identifier formalParameters
        (   methodBody
        |   ';'
        )
    ;

formalParameters
    :   '(' formalParameterList? ')'
    ;

formalParameterList
    :   formalParameter (',' formalParameter)*
    ;

formalParameter
    :   typeType variableDeclaratorId
    ;

methodBody
    :   block
    ;

fieldDeclaration
    :   typeType variableDeclarators ';'
    ;

variableDeclarators
    :   variableDeclarator (',' variableDeclarator)*
    ;

variableDeclarator
    :   variableDeclaratorId
    ;

variableDeclaratorId
    :   Identifier ('[' ']')*
    ;

variableInitializer
    :   arrayInitializer
    |   expression
    ;

arrayInitializer
    :   '{' (variableInitializer (',' variableInitializer)* (',')? )? '}'
    ;

typeType
    :   classOrInterfaceType
    |   primitiveType
    ;

classOrInterfaceType
    :   Identifier ('.' Identifier )*
    ;

primitiveType
    :   'boolean'
    |   'char'
    |   'byte'
    |   'short'
    |   'int'
    |   'long'
    |   'float'
    |   'double'
    ;

literal
    :   IntegerLiteral
    |   FloatingPointLiteral
    |   StringLiteral
    |   'null'
    ;

block
    :   '{' blockStatement* '}'
    ;

blockStatement
    :   localVariableDeclarationStatement
    |   statement
    ;

localVariableDeclarationStatement
    :   localVariableDeclaration ';'
    ;

localVariableDeclaration
    :  typeType variableDeclarators
    ;

statement
    :   block
    |   'if' parExpression statement ('else' statement)?
    |   'while' parExpression statement
    |   'return' expression? ';'
    |   ';'
    |   statementExpression ';'
    |   Identifier ':' statement
    ;

parExpression
    :   '(' expression ')'
    ;

expressionList
    :   expression (',' expression)*
    ;

statementExpression
    :   expression
    ;

expression
    :   primary
    |   expression '.' Identifier
    |   expression '.' 'this'
    |   expression '.' 'super' superSuffix
    |   expression '.' explicitGenericInvocation
    |   expression '[' expression ']'
    |   expression '(' expressionList? ')'
    |   'new' creator
    |   '(' typeType ')' expression
    |   expression ('=') expression
    ;

primary
    :   '(' expression ')'
    |   'this'
    |   'super'
    |   literal
    |   Identifier
    |   typeType '.' 'class'
    |   'void' '.' 'class'
    ;

creator
    :  createdName classCreatorRest
    ;

createdName
    :   Identifier  ('.' Identifier )*
    |   primitiveType
    ;

classCreatorRest
    :   arguments classBody?
    ;

explicitGenericInvocation
    :   explicitGenericInvocationSuffix
    ;

superSuffix
    :   arguments
    |   '.' Identifier arguments?
    ;

explicitGenericInvocationSuffix
    :   'super' superSuffix
    |   Identifier arguments
    ;

arguments
    :   '(' expressionList? ')'
    ;

IntegerLiteral
    :    [0-9]+
    ;

FloatingPointLiteral
    :   [0-9]+ '.' [0-9]+
    ;

StringLiteral
    :   '"' .*? '"'
    ;

Identifier
    :   [a-zA-Z] [a-zA-Z0-9]*
    ;

WS  :  [ \t\r\n\u000C]+ -> skip
    ;

COMMENT
    :   '/*' .*? '*/' -> channel(HIDDEN)
    ;

LINE_COMMENT
    :   '//' ~[\r\n]* -> channel(HIDDEN)
    ;