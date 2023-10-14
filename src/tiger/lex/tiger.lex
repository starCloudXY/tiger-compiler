%filenames = "scanner"
digits [0-9]+
%x COMMENT STR IGNORE ESCAPE

%%

"/*" {
    adjust();
    push(StartCondition__::COMMENT);
}
<COMMENT>{
    "/*" {
        adjustStr();
        push(StartCondition__::COMMENT);
    }
    "*/" {
        adjustStr();
        popStartCondition();
    }
    .|\n {
        adjustStr();
        }
}
["] {
    more();
    control_size = 0;
    push(StartCondition__::STR);
    }
<STR>{
    \"  {
        std::string str = matched(); //matched: "STRING"
        adjustLength(str.size()+control_size);
        str = str.substr(1,str.size()-2);
        setMatched(str);
        popStartCondition();
        return Parser::STRING;
        }
    [[:alnum:]] {
        more();
        }
    [\+\-\*\/\. !] {
        more(); }
    [\\] {
        more();
        push(StartCondition__::ESCAPE);
        }
}
<ESCAPE>{
    [\^][A-Z] {
        more();
        std::string current_string = matched();
        char last_matched_letter = current_string[current_string.size() - 1];
        current_string = current_string.substr(0, current_string.size() - 3); //remove the \^X
        current_string += char(last_matched_letter - 'A' + 1); //add real \^X, remember \^A is 1
        control_size +=2;
        setMatched(current_string);
        popStartCondition();
    }
    ["] {
        more();
        control_size ++;
        std::string str = matched();
        str = str.substr(0, str.size() - 2); //remove the \\ \"
        str += '\"';
        setMatched(str);
        popStartCondition();
    }
    [n] {
        more();
        std::string str = matched();
        str = str.substr(0,str.size()-2);
        str+= '\n';
        control_size++;
        setMatched(str);
        popStartCondition();
        }
    [t] {
        more();
        std::string str = matched(); //matched: "STRING\\t
        str = str.substr(0,str.size()-2);
        str+= '\t';
        control_size++;
        setMatched(str);
        popStartCondition();
        }
    [\\] {
        more();
        std::string str = matched(); //matched: "STRING\\ \\
        str = str.substr(0,str.size()-2);
        str+= '\\';
        control_size++;
        setMatched(str);
        popStartCondition();
        }
    [0-9]{3} {
        more();
        std::string str = matched(); //matched: "STRING\\ NNN
        std::string number_str = str.substr(str.size()-3,str.size());
        str = str.substr(0,str.size()-4);
        control_size+=3;
        int asc_number = atoi(number_str.c_str());
        str += asc_number;
        setMatched(str);
        popStartCondition();
    }
    [\n] {
        more();
        std::string str = matched(); //matched: "STRING\\ \n
        str = str.substr(0,str.size()-2);
        setMatched(str); //setMatched:"STRING
        control_size += 2;
        push(StartCondition__::IGNORE);
        }
}
<IGNORE> {
  \\ {
    more();
    std::string str = matched();//match:"STRING \\
    str = str.substr(0,str.size()-1);
    control_size++;
    setMatched(str);
    popStartCondition();//pop IGNORE
    popStartCondition();//pop ESCAPE
  }
  . {
    more();
    std::string str = matched();//match:"STRING
    str = str.substr(0,str.size()-1);
    control_size++;
    setMatched(str);
    }
}
 /* reserved words */
"array" {adjust(); return Parser::ARRAY;}
"if" {adjust();return Parser::IF;}
"then" {adjust();return Parser::THEN;}
"let" {adjust();return Parser::LET;}
"var" {adjust();return Parser::VAR;}
"type" {adjust();return Parser::TYPE;}
"end" {adjust();return Parser::END;}
"break" {adjust();return Parser::BREAK;}
"function" {adjust();return Parser::FUNCTION;}
"in" {adjust();return Parser::IN;}
"do" {adjust();return Parser::DO;}
"for" {adjust();return Parser::FOR;}
"to" {adjust();return Parser::TO;}
"while" {adjust();return Parser::WHILE;}
"else" {adjust();return Parser::ELSE;}
"of" {adjust();return Parser::OF;}
"nil" {adjust();return Parser::NIL;}

[a-zA-Z][a-z0-9A-Z_]* {
            adjust();
            return Parser::ID;
            }
{digits} {adjust();return Parser::INT;}
"=" {adjust(); return Parser::EQ;}
"<>" {adjust(); return Parser::NEQ;}
"<" {adjust(); return Parser::LT;}
"<=" {adjust(); return Parser::LE;}
">" {adjust(); return Parser::GT;}
">=" {adjust(); return Parser::GE;}
"&" {adjust(); return Parser::AND;}
"|" {adjust(); return Parser::OR;}
":=" {adjust(); return Parser::ASSIGN;}
\, {adjust(); return Parser::COMMA;}
\: {adjust(); return Parser::COLON;}
\; {adjust(); return Parser::SEMICOLON;}
\( {adjust(); return Parser::LPAREN;}
\) {adjust(); return Parser::RPAREN;}
\[ {adjust(); return Parser::LBRACK;}
\] {adjust(); return Parser::RBRACK;}
\{ {adjust(); return Parser::LBRACE;}
\} {adjust(); return Parser::RBRACE;}
\. {adjust(); return Parser::DOT;}
\+ {adjust(); return Parser::PLUS;}
\- {adjust(); return Parser::MINUS;}
\* {adjust(); return Parser::TIMES;}
\/ {adjust(); return Parser::DIVIDE;}

 /*
  * skip white space chars.
  * space, tabs and LF
  */
[ \t]+ {adjust();}
\n {adjust(); errormsg_->Newline();}

 /* illegal input */
. {adjust(); errormsg_->Error(errormsg_->tok_pos_, "illegal token");}
