#include "Compilation.h"

//打印用
static std::string ClassCodeToString(CLASS_CODE c) {
    switch (c)
    {
    case IDENFR:
        return "IDENFR";
    case INTCON:
        return "INTCON";
    case STRCON:
        return "STRCON";
    case MAINTK:
        return "MAINTK";
    case CONSTTK:
        return "CONSTTK";
    case INTTK:
        return "INTTK";
    case BREAKTK:
        return "BREAKTK";
    case CONTINUETK:
        return "CONTINUETK";
    case IFTK:
        return "IFTK";
    case ELSETK:
        return "ELSETK";
    case NOT:
        return "NOT";
    case AND:
        return "AND";
    case OR:
        return "OR";
    case WHILETK:
        return "WHILETK";
    case GETINTTK:
        return "GETINTTK";
    case PRINTFTK:
        return "PRINTFTK";
    case RETURNTK:
        return "RETURNTK";
    case PLUS:
        return "PLUS";
    case MINU:
        return "MINU";
    case VOIDTK:
        return "VOIDTK";
    case MULT:
        return "MULT";
    case DIV:
        return "DIV";
    case MOD:
        return "MOD";
    case LSS:
        return "LSS";
    case LEQ:
        return "LEQ";
    case GRE:
        return "GRE";
    case GEQ:
        return "GEQ";
    case EQL:
        return "EQL";
    case NEQ:
        return "NEQ";
    case ASSIGN:
        return "ASSIGN";
    case SEMICN:
        return "SEMICN";
    case COMMA:
        return "COMMA";
    case LPARENT:
        return "LPARENT";
    case RPARENT:
        return "RPARENT";
    case LBRACK:
        return "LBRACK";
    case RBRACK:
        return "RBRACK";
    case LBRACE:
        return "LBRACE";
    case RBRACE:
        return "RBRACE";
    case END:
        return "END";
    default:
        return "UNKNOWN";
    }
}

void reportSemanticError(const std::string& msg) {
    std::cerr << "[Semantic Error] " << msg << std::endl;
}

/*词法分析器部分*/

//字符串池
std::string StringPool::addString(const std::string& str) {
    if (strToLabel.count(str) == 0) {
        std::string label = "str" + std::to_string(labelCounter++);
        strToLabel[str] = label;
    }
    return strToLabel[str];
}

void StringPool::emit(std::vector<std::string>& strs) {
    for (const auto& strlabel : strToLabel) {
        std::string str = strlabel.first;
        std::string label = strlabel.second;
        std::string emitStr = label + ": .asciiz \"" + escape(str);
        strs.emplace_back(emitStr);
    }
}

bool StringPool::search(std::string s) {
    auto it = strToLabel.find(s);
    if (it != strToLabel.end()) {
        return true;
    }
    else return false;
}

std::string StringPool::getLabel(std::string s) {
    auto it = strToLabel.find(s);
    if (it != strToLabel.end()) {
        return it->second;
    }
    else {
        addString(s);
        it = strToLabel.find(s);
        if (it != strToLabel.end()) {
            return it->second;
        }
    }

}

StringPool strPool;


//解析器
Lexer::Lexer(const std::string& fn) {
    filename = fn;
    nextTokenCached = false;
    secondTokenCached = false;
    ReadFile();
}

void Lexer::ReadFile() {
    std::ifstream fin;
    fin.open(filename, std::ios::in);
    if (!fin.is_open()) {
        std::cerr << "Cannot open the file. " << std::endl;
        exit(-1);
    }
    std::string buffer;
    while (std::getline(fin, buffer)) {
        input += buffer + "\n";
    }
    length = input.length();
    pos = 0;
    if (!length)
        c = '\0';
    else c = input.at(0);
    return;
}

Word Lexer::GetNextToken() {
    Word currentToken;
    if (nextTokenCached) {
        currentToken = nextToken;
        nextTokenCached = false;

        // 如果 secondToken 也缓存过，前移
        if (secondTokenCached) {
            nextToken = secondToken;
            nextTokenCached = true;
            secondTokenCached = false;
        }

        return currentToken;
    }
    currentToken = NextWord();
    return currentToken;
}

Word Lexer::PeekNextToken() {
    if (!nextTokenCached) {
        nextToken = NextWord();
        nextTokenCached = true;
    }
    return nextToken;
}

Word Lexer::Peek2ndToken() {
    if (!nextTokenCached) {
        nextToken = NextWord();
        nextTokenCached = true;
    }
    if (!secondTokenCached) {
        secondToken = NextWord();
        secondTokenCached = true;
    }
    return secondToken;
}

//第一层分选
Word Lexer::NextWord() {
    while (c != '\0') {
        if (isspace(c)) {
            SkipSpace();
            continue;
        }
        if (c == '/' && (input.at(pos + 1) == '/' || input.at(pos + 1) == '*')) {
            SkipComment();
            continue;
        }
        if (c == '"') {
            return Str();
        }
        if (isalpha(c) || c == '_') {
            return Identifier();
        }
        if (isdigit(c)) {
            return Number();
        }
        if (ispunct(c)) {
            return Op();
        }
        Advance();
    }
    Word temp_w("", END);
    return temp_w;
}


//移动索引
void Lexer::Advance() {
    pos++;
    if (pos < length) {
        c = input.at(pos);
    }
    else c = '\0';
    return;
}

    //跳过空格
void Lexer::SkipSpace() {
    while (isspace(c)) Advance();
    return;
}

//跳过注释
void Lexer::SkipComment() {
    if (c == '/') {
        if (pos+1 < length && input.at(pos+1) == '/') {
            while (c != '\n' && c != '\0') {
                Advance();
            }
        }
        else if (pos + 1 < length && input.at(pos + 1) == '*') {
            Advance(); //跳过*
            while (pos < length) {
                if (c == '*' && input.at(pos + 1) == '/') {
                    Advance();
                    Advance();
                    break;
                }
                Advance();
            }
        }
    }
}

//解析关键词，也就是区分keyword和自定义函数
Word Lexer::Identifier() {
    std::string result;
    while (isalnum(c) || c == '_') {
        result += c;
        Advance();
    }
    if (result == "if") {
        return Word(result, IFTK);
    }
    else if (result == "while") {
        return Word(result, WHILETK);
    }
    else if (result == "break") {
        return Word(result, BREAKTK);
    }
    else if (result == "continue") {
        return Word(result, CONTINUETK);
    }
    else if (result == "const") {
        return Word(result, CONSTTK);
    }
    else if (result == "getint") {
        return Word(result, GETINTTK);
    }
    else if (result == "int") {
        return Word(result, INTTK);
    }
    else if (result == "else") {
        return Word(result, ELSETK);
    }
    else if (result == "printf") {
        return Word(result, PRINTFTK);
    }
    else if (result == "return") {
        return Word(result, RETURNTK);
    }
    else if (result == "void") {
        return Word(result, VOIDTK);
    }
    else if (result == "main") {
        return Word(result, MAINTK);
    }
    else return Word(result, IDENFR);
}

//解析数字
Word Lexer::Number() {
    std::string result;
    while (isdigit(c)) {
        result += c;
        Advance();
    }
    Word temp_w(result, INTCON);
    return temp_w;
}

//解析符号
Word Lexer::Op() {
    std::string result(1,c);
    Advance();
    if (result == "!") {
        if (c == '=') {
            Advance();
            return Word(result + '=', NEQ);
        }
        return Word(result, NOT);
    }
    else if (result == "-") {
        return Word(result, MINU);
    }
    else if (result == "=") {
        if (c == '=') {
            Advance();
            return Word(result + '=', EQL);
        }
        return Word(result, ASSIGN);
    }
    else if (result == "+") {
        return Word(result, PLUS);
    }
    else if (result == "*") {
        return Word(result, MULT);
    }
    else if (result == "/") {
        return Word(result, DIV);
    }
    else if (result == "%") {
        return Word(result, MOD);
    }
    else if (result == "<") {
        if (c == '=') {
            Advance();
            return Word(result + '=', LEQ);
        }
        return Word(result, LSS);
    }
    else if (result == ">") {
        if (c == '=') {
            Advance();
            return Word(result + '=', GEQ);
        }
        return Word(result, GRE);
    }
    else if (result == ";") {
        return Word(result, SEMICN);
    }
    else if (result == ",") {
        return Word(result, COMMA);
    }
    else if (result == "(") {
        return Word(result, LPARENT);
    }
    else if (result == ")") {
        return Word(result, RPARENT);
    }
    else if (result == "[") {
        return Word(result, LBRACK);
    }
    else if (result == "]") {
        return Word(result, RBRACK);
    }
    else if (result == "{") {
        return Word(result, LBRACE);
    }
    else if (result == "}") {
        return Word(result, RBRACE);
    }
    else if (result == "&" && c == '&') {
        Advance();
        return Word(result + '&', AND);
    }
    else if (result == "|" && c == '|') {
        Advance();
        return Word(result + '|', OR);
    }
    else {
        return Word(result, UNKNOWN);
    } //错误处理
}

//解析字符串
Word Lexer::Str() {
    std::string result;
    Advance(); //跳过开头的"
    while (pos < length && c != '\0' && c != '"') {
        //处理转义字符
        if (c == '\\') {
            Advance();
            if (c == 'n') result += "\\n";
            else result += c;
        }
        else result += c;
        Advance();
    }
    Advance(); //跳过结尾的"
    //放入字符串池
    strPool.addString(result);

    return Word(result, STRCON);
}


/*语法分析器部分*/
/*附带语义分析*/
SymbolTable symTable;

Parser::Parser(Lexer& l, std::ofstream& fout) : lexer(l), fout(fout) {
    currentToken = lexer.GetNextToken();
}

//进入解析的主函数，CompUnit
std::unique_ptr<ASTNode> Parser::Parse() {
    return ParseCompUnit();
}

void Parser::PrintSymbol(const std::string& symbol) {
    if(IFDEBUG) fout << symbol << std::endl;
}

void Parser::PrintToken(Word& word) {
    if (IFDEBUG) {
        if (word.getcode() == STRCON) {
            fout << ClassCodeToString(word.getcode()) << " " << "\"" << word.getword() << "\"" << std::endl;
        }
        else fout << ClassCodeToString(word.getcode()) << " " << word.getword() << std::endl;
    }
}

void Parser::Advance() {
    if (currentToken.getcode() != END) {
        PrintToken(currentToken);
    }
    currentToken = lexer.GetNextToken();
}

std::unique_ptr<ASTNode> Parser::ParseDecl(bool ifFunc) {
    std::unique_ptr<ASTNode> node;
    if (currentToken.getcode() == CONSTTK) {
        node = ParseConstDecl();
    }
    else if (currentToken.getcode() == INTTK) {
        node = ParseVarDecl();
    }
    else {
        reportSemanticError("Expected a declaration (const or var)!");
    }
    return node;
}

std::unique_ptr<ASTNode> Parser::ParseBType() {
    if (currentToken.getcode() == INTTK) {
        Advance(); //跳过int
        return std::unique_ptr<BTypeNode>(new BTypeNode("int"));
    }
    reportSemanticError("Expected 'int' for BType");
    return std::unique_ptr<BTypeNode>();
}

std::unique_ptr<ASTNode> Parser::ParseConstDecl() {
    if (currentToken.getcode()!=CONSTTK) {
        reportSemanticError("Expected 'const'");
    }
    Advance();

    curBType = "const int";
    auto constDecl = std::unique_ptr<ConstDeclNode>(new ConstDeclNode(ParseBType()));
    constDecl->constdefs.push_back(std::move(ParseConstDef()));

    while (currentToken.getcode() == COMMA) {
        Advance(); //跳过逗号
        constDecl->constdefs.push_back(std::move(ParseConstDef()));
    }

    if (currentToken.getcode() != SEMICN) {
        reportSemanticError("Expected ';' at the end of const declaration");
    }
    Advance(); //跳过；
    PrintSymbol("<ConstDecl>");
    return constDecl;
}

std::unique_ptr<ASTNode> Parser::ParseConstDef() {
    if (currentToken.getcode() != IDENFR) {
        reportSemanticError("Expected identifier in const definition");
    }

    std::string name = currentToken.getword(); //idenfr的名字
    Advance(); //跳过标识符

    //可选的数组
    std::unique_ptr<ASTNode> constExp = nullptr;
    int arraySize = 1;
    bool isArray = false;

    if (currentToken.getcode() == LBRACK) {
        Advance();
        constExp = ParseConstExp();

        arraySize = std::atoi(constExp->value.c_str());
        if (arraySize <= 0) {
            reportSemanticError("Must be a natual number for array size!");
        }
        isArray = true;

        if (currentToken.getcode() != RBRACK) {
            reportSemanticError("Expected ']' after ConstExp");
        }
        Advance();
    }

    //匹配=
    if (currentToken.getcode() != ASSIGN) {
        reportSemanticError("Expected \'=\' in const definition");
    }
    Advance();

    auto initVal = ParseConstInitVal();

    //尝试加入符号表
    if (!symTable.addSymbol(name, CONSTVAR, curBType)) {
        reportSemanticError("Redefinition in ConstDef");
    }
    IR::isFunction = 0;

    PrintSymbol("<ConstDef>");
    return std::unique_ptr<ConstDefNode>(new ConstDefNode(name, std::move(constExp), std::move(initVal)));
}

std::unique_ptr<ASTNode> Parser::ParseConstInitVal() {
    std::vector<std::unique_ptr<ASTNode>> values;
    if (currentToken.getcode() == LBRACE) {
        Advance();
        if (currentToken.getcode() != RBRACE) {
            values.push_back(ParseConstInitVal());

            while (currentToken.getcode() == COMMA) {
                Advance(); //跳过逗号
                values.push_back(ParseConstInitVal());
            }
            //至少有一个ConstInitVal
        }
        if (currentToken.getcode() != RBRACE) {
            reportSemanticError("Expected '}' after ConstInitVal list");
        }
        Advance(); //跳过}
    }
    else {
        //匹配ConstExp
        values.push_back(ParseConstExp());
    }
    PrintSymbol("<ConstInitVal>");
    return std::unique_ptr<ConstInitValNode>(new ConstInitValNode(std::move(values)));
}

std::unique_ptr<ASTNode> Parser::ParseVarDecl() {
    auto btype = ParseBType();
    curBType = "int";

    std::vector<std::unique_ptr<ASTNode>> defs;
        
    defs.push_back(std::move(ParseVarDef()));
        
    while (currentToken.getcode() == COMMA) {
        Advance();
        defs.push_back(std::move(ParseVarDef()));
    }

    if (currentToken.getcode() != SEMICN) {
        reportSemanticError("Expected ';' after VarDecl");
    }
    Advance(); //跳过;
    PrintSymbol("<VarDecl>");
    return std::unique_ptr<VarDeclNode>(new VarDeclNode(std::move(defs)));
}

std::unique_ptr<ASTNode> Parser::ParseVarDef() {
    if (currentToken.getcode() != IDENFR) {
        reportSemanticError("Expected identifier in VarDef");
    }
    std::string name = currentToken.getword();
    Advance(); //跳过idenfr
    std::unique_ptr<ASTNode> constExp = nullptr;
    std::unique_ptr<ASTNode> init = nullptr;
    std::string type = curBType;

    //语义分析
    bool isArray = false;
    int arraySize = 1;

    //判断是否是数组定义
    if (currentToken.getcode() == LBRACK) {
        Advance(); //跳过[
        constExp = ParseConstExp();
        isArray = true;

        //确认数组大小是正整数常量
        int arraysize = std::atoi(constExp->value.c_str());
        if (arraysize <= 0) {
            reportSemanticError("Must be a natual number for array size!");
        }


        if (currentToken.getcode() != RBRACK) {
            reportSemanticError("Expected ']' in array VarDef");
        }
        Advance(); //跳过]
    }

    //判断是否初始化
    if (currentToken.getcode() == ASSIGN) {
        Advance(); //跳过=
        init = ParseInitVal();
    }

    //加入符号表
    SymbolKind kind = VAR;
    if (!symTable.addSymbol(name, kind, type)) {
        reportSemanticError("Redefinition in VarDef!");
    }
    IR::isFunction = 0;

    PrintSymbol("<VarDef>");
    return std::unique_ptr<VarDefNode>(new VarDefNode(name, std::move(constExp), std::move(init)));
}

std::unique_ptr<ASTNode> Parser::ParseInitVal() {
    if (currentToken.getcode() == LBRACE) {
        Advance();
        std::vector<std::unique_ptr<ASTNode>> values;
        if (currentToken.getcode() != RBRACE) {
            values.push_back(ParseInitVal());
            while (currentToken.getcode() == COMMA) {
                Advance(); //跳过,
                values.push_back(ParseInitVal());
            }
        }
        if (currentToken.getcode() != RBRACE) {
            reportSemanticError("Expected '}' in InitVal");
        }
        Advance(); //跳过}
        PrintSymbol("<InitVal>");
        return std::unique_ptr<InitValNode>(new InitValNode(std::move(values)));
    }
    else {
        auto expr = ParseExp();
        PrintSymbol("<InitVal>");
        return std::unique_ptr<InitValNode>(new InitValNode(std::move(expr)));
    }
}

std::unique_ptr<ASTNode> Parser::ParseAddExp() {
    auto left = ParseMulExp();

    //有左递归，但是本质上是MulExp不断加减，所以可以这么写
    while (currentToken.getcode() == PLUS || currentToken.getcode() == MINU) {
        PrintSymbol("<AddExp>");
        std::string op = currentToken.getword();
        Advance();
        auto right = ParseMulExp();
        
        //生成四元式代码
        std::string temp = IR::newTemp();
        IR::emit(op, left->value, right->value, temp);

        left = std::unique_ptr<AddExpNode>(new AddExpNode(std::move(left), op, std::move(right)));
        left->value = temp;
    }

    PrintSymbol("<AddExp>");
    return left;
}

std::unique_ptr<ASTNode> Parser::ParseMulExp() {
    auto left = ParseUnaryExp();

    while (currentToken.getcode() == MULT || currentToken.getcode() == DIV || currentToken.getcode() == MOD) {
        PrintSymbol("<MulExp>");
        std::string op = currentToken.getword();
        Advance();
        auto right = ParseUnaryExp();

        std::string temp = IR::newTemp();
        IR::emit(op, left->value, right->value, temp);

        left = std::unique_ptr<MulExpNode>(new MulExpNode(std::move(left), op, std::move(right)));
        left->value = temp;
    }

    PrintSymbol("<MulExp>");
    return left;
}

std::unique_ptr<ASTNode> Parser::ParseUnaryExp() {
    std::unique_ptr<ASTNode> node;
    if (currentToken.getcode() == LPARENT || currentToken.getcode() == INTCON) {
        node=ParsePrimaryExp();
    }
    else if (currentToken.getcode() == IDENFR) {
        if (lexer.PeekNextToken().getcode() == LPARENT) {
            node=ParseFuncCall();
        }
        else node=ParsePrimaryExp();

    }
    else if (currentToken.getcode() == PLUS || currentToken.getcode() == MINU || currentToken.getcode() == NOT) {
        Advance();
        PrintSymbol("<UnaryOp>");
        node=ParseUnaryExp();

        std::string temp = IR::newTemp();
        std::string op = currentToken.getword(); //获取运算符
        if (op == "-") {
            IR::emit("neg", "0", node->value, temp);
        }
        else if (op == "!") {
            IR::emit("!", node->value, "_", temp);
        }
        else {
            temp=node->value;
        }

        node->value = temp;
    }
    else {
        reportSemanticError("No Expected Symbol in UnaryExp");
    }
    PrintSymbol("<UnaryExp>");
    return node;
}

std::unique_ptr<ASTNode> Parser::ParsePrimaryExp() {
    if (currentToken.getcode() == LPARENT) {
        Advance();
        auto expr = ParseExp();
        if (currentToken.getcode() != RPARENT) {
            reportSemanticError("Expected ')' in PrimaryExp");
        }
        Advance();
        PrintSymbol("<PrimaryExp>");
        return std::unique_ptr<PrimaryExpNode>(new PrimaryExpNode(std::move(expr)));
    }
    if (currentToken.getcode() == IDENFR) {
        auto node = ParseLVal();
        PrintSymbol("<PrimaryExp>");
        return node;
    }
    if (currentToken.getcode() == INTCON) {
        int value = std::stoi(currentToken.getword());
        Advance();
        auto node= std::unique_ptr<PrimaryExpNode>(new PrimaryExpNode(std::unique_ptr<NumberNode>(new NumberNode(value))));
        node->value = "$" + value; //立即数，用"$"标识
        PrintSymbol("<Number>");
        PrintSymbol("<PrimaryExp>");
        return node;
    }
    throw std::runtime_error("Invalid primary expression");
    return nullptr;
}

std::unique_ptr<ASTNode> Parser::ParseLVal(bool ifFunc) {
    if (currentToken.getcode() != IDENFR) {
        reportSemanticError("Expected identifier in LVal");
    }
    std::string varName = currentToken.getword();
    Advance();

    if (!symTable.lookup(varName)) {
        reportSemanticError("Undeclared variable: " + varName);
    }

    //处理数组下标
    if (currentToken.getcode() == LBRACK) {
        Advance();
        auto index = ParseExp();
        if (currentToken.getcode() != RBRACK) {
            reportSemanticError("Expected ']' after array index");
        }
        Advance();
        PrintSymbol("<LVal>");
        return std::unique_ptr<LValNode>(new LValNode(varName, std::move(index)));
    }
    else {
        //没有数组下表，基本变量
        PrintSymbol("<LVal>");
        auto node = std::unique_ptr<LValNode>(new LValNode(varName));
        node->value = varName; //变量本身就是值名

        return node;
    }
}

std::unique_ptr<ASTNode> Parser::ParseExp(bool ifFunc) {
    auto node = ParseAddExp();
    PrintSymbol("<Exp>");
    return node;
}

std::unique_ptr<ASTNode> Parser::ParseRelExp() {
    auto left = ParseAddExp();
    while (currentToken.getcode() == LSS || currentToken.getcode() == GRE || currentToken.getcode() == LEQ || currentToken.getcode() == GEQ) {
        PrintSymbol("<RelExp>");
        std::string op = currentToken.getword(); //记录操作符
        Advance();
        auto right = ParseAddExp();

        std::string temp = IR::newTemp();
        IR::emit(op, left->value, right->value, temp);

        left = std::unique_ptr<RelExpNode>(new RelExpNode(op, std::move(left), std::move(right)));
        left->value = temp;
    }
    PrintSymbol("<RelExp>");
    return left;
}

std::unique_ptr<ASTNode> Parser::ParseConstExp() {
    auto node = ParseAddExp();
    PrintSymbol("<ConstExp>");
    return node;
    //标识符必须是常量，后续需要加入语义约束
}

std::unique_ptr<ASTNode> Parser::ParseFuncRParams() {
    auto paramsNode = std::unique_ptr<FuncRParamsNode>(new FuncRParamsNode());
    auto node = ParseExp();
    IR::emit("param", node->value, "_", "_");
    paramsNode->addArg(std::move(node));

    while (currentToken.getcode() == COMMA) {
        Advance();
        auto node = ParseExp();
        IR::emit("param", node->value, "_", "_");
        paramsNode->addArg(std::move(node));
    }

    PrintSymbol("<FuncRParams>");
    return paramsNode;
}

std::unique_ptr<ASTNode> Parser::ParseEqExp() {
    auto left = ParseRelExp();
    while (currentToken.getcode() == EQL || currentToken.getcode() == NEQ) {
        PrintSymbol("<EqExp>");
        std::string op = currentToken.getword();
        Advance();
        auto right = ParseRelExp();

        std::string temp = IR::newTemp();
        IR::emit(op, left->value, right->value, temp);

        left = std::unique_ptr<EqExpNode>(new EqExpNode(op, std::move(left), std::move(right)));
        left->value = temp;
    }
    PrintSymbol("<EqExp>");
    return left;
}

std::unique_ptr<ASTNode> Parser::ParseLAndExp() {
    auto left = ParseEqExp();

    while (currentToken.getcode() == AND) {
        PrintSymbol("<LAndExp>");
        Advance(); //跳过&&
        auto right = ParseEqExp();

        std::string temp = IR::newTemp();
        IR::emit("AND", left->value, right->value, temp);

        left = std::unique_ptr<LAndExpNode>(new LAndExpNode(std::move(left), std::move(right)));
        left->value = temp;
    }
    PrintSymbol("<LAndExp>");
    return left;
}

std::unique_ptr<ASTNode> Parser::ParseLOrExp(bool ifFunc) {
    auto left = ParseLAndExp();
    while (currentToken.getcode() == OR) {
        PrintSymbol("<LOrExp>");
        Advance();
        auto right = ParseLAndExp();

        std::string temp = IR::newTemp();
        IR::emit("OR", left->value, right->value, temp);

        left = std::unique_ptr<LOrExpNode>(new LOrExpNode(std::move(left), std::move(right)));
        left->value = temp;
    }
    PrintSymbol("<LOrExp>");
    return left;
}

//语法里没有，但是函数调用情况比较多所以打包了
std::unique_ptr<ASTNode> Parser::ParseFuncCall() {
    std::string funcName = currentToken.getword();
    Advance();

    if (currentToken.getcode() != LPARENT) {
        reportSemanticError("Expected '(' after function name");
    }

    Advance();
    
    auto funcCallNode = std::unique_ptr<FuncCallNode>(new FuncCallNode(funcName));
    
    //函数查表
    std::shared_ptr<Symbol> funcSym = symTable.lookup(funcName);
    if (!funcSym || funcSym->getKind() != FUNC) {
        reportSemanticError("Undefined Function");
    }
    //函数参数语义分析
    const auto& formalParams = funcSym->getParams();
    
    if (currentToken.getcode() != RPARENT) {
        while (true) {
            auto param = ParseFuncRParams();
            funcCallNode->addParams(std::move(param));
            if (currentToken.getcode() == COMMA) {
                Advance(); //跳过逗号，解析下一个参数
            }
            else break;
        }
    }

    if (currentToken.getcode() != RPARENT) {
        reportSemanticError("Expected ')' after function parameters");
    }
    Advance();

    //参数数量检查
    if (funcCallNode->paramNum() != formalParams.size()) {
        reportSemanticError("Function arguments dismatched");
    }

    std::string temp = IR::newTemp();
    IR::emit("CALL", funcName, std::to_string(funcCallNode->paramNum()), temp);
    funcCallNode->value = temp;

    return funcCallNode;
}

std::unique_ptr<ASTNode> Parser::ParseFuncDef() {
    std::string returnType = ParseFuncType();
    curFuncType = returnType;

    if (currentToken.getcode() != IDENFR) {
        reportSemanticError("Expected function name after return type");
    }
    std::string funcName = currentToken.getword();
    Advance();

    if (currentToken.getcode() != LPARENT) {
        reportSemanticError("Expected '(' after function name");
    }
    Advance();

    std::vector<std::unique_ptr<ASTNode>> params;
    std::vector<std::pair<std::string, std::string>> paramstr;
    if (currentToken.getcode() != RPARENT) {
        params = ParseFuncFParams();

        for (const auto& param : params) {
            auto* paramNode = dynamic_cast<FuncFParamNode*>(param.get());
            if (!paramNode) {
                reportSemanticError("Invalid parameter node type");
            }
            std::string typeStr = extractTypeString(paramNode->type);
            std::string name = paramNode->name;
            paramstr.emplace_back(name, typeStr);
        }

    }

    if (currentToken.getcode() != RPARENT) {
        reportSemanticError("Expected ')' after parameter list");
    }
    Advance();

    //注册函数+参数
    std::unordered_map<std::string, int> paramOffsets;
    bool success = symTable.addFunction(funcName, returnType, paramstr, paramOffsets);
    if (!success) {
        reportSemanticError("Function redeclared: " + funcName);
    }
    std::shared_ptr<Symbol> sym = symTable.tableStack[0][funcName];
    IR::isFunction = 1;
    IR::func = sym;
    
    symTable.enterScope();
    for (const auto& par : paramstr) {
        std::string name = par.first;
        std::string type = par.second;
        if (!symTable.addSymbol(name, VAR, type)) {
            reportSemanticError("Redefinition in FuncDef");
        }//为每个形参手动注册
    }

    auto block = ParseBlock(1); //函数体

    symTable.exitScope();

    IR::isFunction = 0;
    PrintSymbol("<FuncDef>");
    return std::unique_ptr<FuncDefNode>(new FuncDefNode(returnType, funcName, std::move(params), std::move(block)));
}

std::string Parser::ParseFuncType() {
    if (currentToken.getcode() == INTTK||currentToken.getcode()==VOIDTK) {
        std::string type = currentToken.getword();
        Advance();
        PrintSymbol("<FuncType>");
        return type;
    }
    else {
        reportSemanticError("Expected symbol in FuncType");
    }
}

std::vector<std::unique_ptr<ASTNode>> Parser::ParseFuncFParams() {
    std::vector<std::unique_ptr<ASTNode>> params;
    params.push_back(ParseFuncFParam());
    while (currentToken.getcode() == COMMA) {
        Advance();
        params.push_back(ParseFuncFParam());
    }
    PrintSymbol("<FuncFParams>");
    return params;
}

std::unique_ptr<ASTNode> Parser::ParseFuncFParam() {
        auto type = ParseBType();
        if (currentToken.getcode() != IDENFR) {
        reportSemanticError("Expected identifier in function parameter");
        exit(1);
        }
        std::string name = currentToken.getword();
        Advance();
        PrintSymbol("<FuncFParam>");
        return std::unique_ptr<FuncFParamNode>(new FuncFParamNode(std::move(type), name));
}

std::unique_ptr<ASTNode> Parser::ParseBlock(bool ifFunc) {
    if (currentToken.getcode() != LBRACE) {
        reportSemanticError("Expected '{' at beginning of block");
        exit(1);
    }
    Advance();

    auto block = std::unique_ptr<BlockNode>(new BlockNode());

    while (currentToken.getcode() != RBRACE) {
        auto item = ParseBlockItem(ifFunc);
        if (item) {
            block->addItem(std::move(item));
        }
        else {
            reportSemanticError("Invalid block item");
        }
    }
    Advance(); //跳过}
    PrintSymbol("<Block>");
    return block;
}

std::unique_ptr<ASTNode> Parser::ParseBlockItem(bool ifFunc) {
    std::unique_ptr<ASTNode> node;
    if (currentToken.getcode() == INTTK || currentToken.getcode() == CONSTTK) {
        node = ParseDecl(ifFunc);
    }
    else node = ParseStmt(ifFunc); //这行吗
    return node;
}

std::unique_ptr<ASTNode> Parser::ParseStmt(bool ifFunc) {
    std::unique_ptr<ASTNode> node;
    if (currentToken.getcode() == LBRACE) {
        node = ParseBlock(ifFunc);
    }
    else if (currentToken.getcode() == IFTK) {
        node = ParseIfStmt(ifFunc);
    }
    else if (currentToken.getcode() == WHILETK) {
        node = ParseWhileStmt(ifFunc);
    }
    else if (currentToken.getcode() == BREAKTK) {
        Advance();
        Expect(SEMICN);
        node = std::unique_ptr<BreakNode>(new BreakNode());
    }
    else if (currentToken.getcode() == CONTINUETK) {
        Advance();
        Expect(SEMICN);
        node = std::unique_ptr<ContinueNode>(new ContinueNode());
    }
    else if (currentToken.getcode() == RETURNTK) {
        Advance();
        if (currentToken.getcode() != SEMICN) {
            if (curFuncType == "void") {
                reportSemanticError("VOID Function should not return anything");
            }
            auto expr = ParseExp(ifFunc);
            
            IR::emit("RETURN", expr->value, "", "");

            Expect(SEMICN);
            node = std::unique_ptr<ReturnNode>(new ReturnNode(std::move(expr)));
        }
        else {
            if (curFuncType == "int") {
                reportSemanticError("INT Function should return an integer");
            }
            Advance();

            IR::emit("RETURN", "", "", "");

            node = std::unique_ptr<ReturnNode>(new ReturnNode(nullptr));
        }
    }
    else if (currentToken.getcode() == IDENFR) {
        auto lval = ParseLVal(ifFunc);

        //检查变量是否定义
        std::shared_ptr<Symbol> sym = symTable.lookup(dynamic_cast<LValNode*>(lval.get())->getName());
        if (!sym) {
            reportSemanticError("Using Undefined Variety");
        }
        else if (sym->getKind() == CONSTVAR) {
            reportSemanticError("Const Variety cannot be a LVAL");
        }

        Expect(ASSIGN);
        if (currentToken.getcode() == GETINTTK) {
            Advance();
            Expect(LPARENT);
            Expect(RPARENT);
            Expect(SEMICN);

            //生成四元式代码部分
            auto getintnode = std::unique_ptr<GetintNode>(new GetintNode());
            std::string temp = IR::newTemp();
            IR::emit("READ", "", "", temp);
            getintnode->value = temp; //为上层表达式提供结果

            IR::emit(":=", getintnode->value, "", lval->value);
            std::string tempStr = getintnode->value;
            node = std::unique_ptr<AssignNode>(new AssignNode(std::move(lval), std::move(getintnode), true));
            node->value = tempStr;
        }
        else {
            auto expr = ParseExp(ifFunc);
            IR::emit(":=", expr->value, "", lval->value);
            Expect(SEMICN);
            node = std::unique_ptr<AssignNode>(new AssignNode(std::move(lval), std::move(expr), false));
        }
    }
    else if (currentToken.getcode() == PRINTFTK) {
        Advance();
        Expect(LPARENT);
        std::string formatStr = currentToken.getword();
        Expect(STRCON);
            
        std::vector<std::unique_ptr<ASTNode>> args;
        while (currentToken.getcode() == COMMA) {
            Advance();
            args.push_back(ParseExp(ifFunc));
        }
        Expect(RPARENT);
        Expect(SEMICN);

        node = std::unique_ptr<PrintfNode>(new PrintfNode(formatStr, std::move(args)));

        //生成四元式
        generateIRForPrintf(node);
    
    }
    else if (currentToken.getcode() == SEMICN) {
        Advance(); //空语句
        node = std::unique_ptr<ExprStmtNode>(new ExprStmtNode(nullptr));
    }
    else {
        auto expr = ParseExp(ifFunc);
        Expect(SEMICN);
        node = std::unique_ptr<ExprStmtNode>(new ExprStmtNode(std::move(expr)));
    }
    PrintSymbol("<Stmt>");
    return node;
}

void Parser::Expect(CLASS_CODE expectedcode) {
    if (currentToken.getcode() != expectedcode) {
        reportSemanticError("Syntax Error: Unexpected token '" + currentToken.getword() + "'");
    }
    Advance(); //向前读取下一个token
}

std::unique_ptr<ASTNode> Parser::ParseIfStmt(bool ifFunc) {
    Expect(IFTK);
    Expect(LPARENT);
    auto cond = ParseCond(ifFunc);
    Expect(RPARENT);

    //条件为假时跳转
    std::string labelFalse = IR::newLabel();
    IR::emit("BZ", cond->value, "", labelFalse);

    symTable.enterScope();
    auto thenStmt = ParseStmt(ifFunc);
    symTable.exitScope();

    std::unique_ptr<ASTNode> elseStmt = nullptr;
    
    if (currentToken.getcode() == ELSETK) {
        Advance();

        symTable.enterScope();
        elseStmt = ParseStmt();
        symTable.exitScope();

        std::string labelEnd = IR::newLabel();
        IR::emit("GOTO", "", "", labelEnd);
        IR::emit("LABEL", labelFalse, "", "");
        IR::emit("LABEL", labelEnd, "", "");
    }
    else IR::emit("LABEL", labelFalse, "", "");
    return std::unique_ptr<IfNode>(new IfNode(std::move(cond), std::move(thenStmt), std::move(elseStmt)));
}

std::unique_ptr<ASTNode> Parser::ParseCond(bool ifFunc) {
    auto node = ParseLOrExp(ifFunc);
    PrintSymbol("<Cond>");
    return node;
}

std::unique_ptr<ASTNode> Parser::ParseWhileStmt(bool ifFunc) {
    Expect(WHILETK);
    Expect(LPARENT);

    std::string labelCond = IR::newLabel();
    std::string labelBody = IR::newLabel();
    std::string labelEnd = IR::newLabel();

    IR::emit("LABEL", labelCond, "", "");

    auto cond = ParseCond(ifFunc);
    Expect(RPARENT);

    IR::emit("BZ", cond->value, "", labelEnd); //条件失败跳出
    IR::emit("LABEL", labelBody, "", ""); 

    symTable.enterScope();
    auto body = ParseStmt(ifFunc);
    symTable.exitScope();

    IR::emit("GOTO", "", "", labelCond); //回去重新判断
    IR::emit("LABEL", labelEnd, "", "");

    return std::unique_ptr<WhileNode>(new WhileNode(std::move(cond), std::move(body)));
}

std::unique_ptr<ASTNode> Parser::ParseCompUnit() {
    auto compNode = std::unique_ptr<CompUnitNode>(new CompUnitNode());

    while (currentToken.getcode() != END) {
        if (currentToken.getcode() == CONSTTK) {
            compNode->decls.push_back(ParseDecl());
        }
        else if (currentToken.getcode() == VOIDTK) {
            compNode->funcs.push_back(ParseFuncDef());
        }
        else if (currentToken.getcode() == INTTK) {
            // 需要提前看两个 token
            Word nextToken = lexer.PeekNextToken();
            if (nextToken.getcode() == MAINTK) {
                compNode->mainfunc = ParseMainFuncDef();
                break; // main 函数必须是最后一个
            }
            else {
                Word thirdToken = lexer.Peek2ndToken();
                if (thirdToken.getcode() == LPARENT) {
                    compNode->funcs.push_back(ParseFuncDef());
                }
                else {
                    compNode->decls.push_back(ParseDecl());
                }
            }
        }
        else {
            std::cerr << "Unknown start of definition at token: " << currentToken.getword() << std::endl;
            exit(1);
        }
    }
    PrintSymbol("<CompUnit>");
    return compNode;
}


std::unique_ptr<ASTNode> Parser::ParseMainFuncDef() {
    Expect(INTTK);
    Expect(MAINTK);
    Expect(LPARENT);            
    Expect(RPARENT);            
    auto block = ParseBlock();   
    PrintSymbol("<MainFuncDef>");
    return std::unique_ptr<MainFuncDefNode>(new MainFuncDefNode(std::move(block)));
}

/*语义分析与中间代码生成部分*/

std::shared_ptr<Symbol> IR::func = nullptr;
bool IR::isFunction = false;


//辅助函数，从 ASTNode* 中获取变量名用于生成中间代码
std::string extractTypeString(const std::unique_ptr<ASTNode>& node) {
    if (auto* btype = dynamic_cast<BTypeNode*>(node.get())) {
        return btype->type;
    }
    if (auto* ftype = dynamic_cast<FuncTypeNode*>(node.get())) {
        return ftype->type;
    }
    if (auto* bin = dynamic_cast<const AddExpNode*>(node.get())) {
        return bin->value;
    }
    if (auto* bin = dynamic_cast<const MulExpNode*>(node.get())) {
        return bin->value;
    }
    if (auto* var = dynamic_cast<const VarDefNode*>(node.get())) {
        return var->value;
    }
    if (auto* con = dynamic_cast<const ConstDefNode*>(node.get())) {
        return con->value;
    }
    reportSemanticError("Unsupported type node in extractTypeString");
}

void generateIRForPrintf(const std::unique_ptr<ASTNode>& node) {
    if (auto* pnode = dynamic_cast<PrintfNode*>(node.get())) {
        const std::string& fmt = pnode->formatString;

        int expIndex = 0;
        for (size_t i = 0; i < fmt.length(); ++i) {
            if (fmt[i] == '%' && fmt[i + 1] == 'd') {
                if (expIndex < pnode->args.size()) {
                    std::string val = pnode->args[expIndex]->value;
                    IR::emit("WRITE", val, "", "");
                    ++expIndex;
                    ++i; //跳过d
                }
            }
            else {
                //普通字符或字符串常量，收集
                std::string literal;
                while (i < fmt.length() && !(fmt[i] == '%' && fmt[i + 1] == 'd')) {
                    literal += fmt[i];
                    ++i;
                }
                --i; //回退到%
                IR::emit("WRITE", "\"" + literal + "\"", "", "");
            }
        }

    }
    else {
        reportSemanticError("Cannot convert ASTNode to PrintfNode");
    }
}


/*===================================符号表部分============================================*/

Symbol& Symbol::operator=(const Symbol& other) {
    if (this != &other) {
        name = other.name;
        kind = other.kind;
        type = other.type;
        level = other.level;
        address = other.address;
        isDefined = other.isDefined;
    }
    return *this;
}

void Symbol::paramCopy(std::vector<std::pair<std::string, std::string>> p) {
    params = p;
}

const std::vector<std::pair<std::string, std::string>>& Symbol::getParams() const {
    return params;
}



//符号表管理类
SymbolTable::SymbolTable() {
    tableStack.emplace_back(); //初始化全局作用域
    offsetStack.push(0);
    currentLevel = 0;
}

void SymbolTable::enterScope() {
    currentLevel++;
    tableStack.emplace_back();
    offsetStack.push(0);
}

void SymbolTable::exitScope() {
    if (tableStack.size() > 1) {
        tableStack.pop_back();
        if (!offsetStack.empty()) {
            offsetStack.pop();
        }
        if (currentLevel >= 0) {
            currentLevel--;
        }
    }
    else if (tableStack.empty()) {
        std::cerr << "[SymbolTable Error] Attempted to exit scope from an already empty tableStack!" << std::endl;
    }
}

bool SymbolTable::addSymbol(const std::string& name, SymbolKind kind, const std::string& type) {
    auto& currTable = tableStack.back();
    if (currTable.count(name)) return false; //重定义
    int& currentOffset = offsetStack.top();
    currTable[name] = std::shared_ptr<Symbol>(new Symbol(name, kind, type, currentLevel, currentOffset++));
    return true;
}

std::shared_ptr<Symbol> SymbolTable::lookup(const std::string& name) {
    for (int i = currentLevel; i >= 0; --i) {
        auto& scope = tableStack[i];
        auto it = scope.find(name);
        if (it != scope.end()) {
            return it->second;
        }
    }
    return nullptr;
}

bool SymbolTable::addFunction(const std::string & name, const std::string & returnType,
    const std::vector<std::pair<std::string, std::string>>& paramList,
    std::unordered_map<std::string, int>& outParamOffsets) {
    auto& currTable = tableStack[0]; //全局作用域
    if (currTable.count(name)) {
        std::cerr<< "Error: Function " << name << " already declared."<<std::endl;
        return false; //不考虑全局函数以外的函数，重定义
    }
    std::shared_ptr<Symbol> funcSym(new Symbol(name, FUNC, returnType, 0, -1)); //全局级别地址设为-1
    funcSym->paramCopy(paramList);
    currTable[name] = funcSym;

    //把参数作为局部变量插入符号表
    outParamOffsets = enterFunctionBody(funcSym);

    return true;
}

std::unordered_map<std::string, int> SymbolTable::enterFunctionBody(std::shared_ptr<Symbol> funcSymbol) {
    std::unordered_map<std::string, int> paramOffsets;
    enterScope();
    int currentOffset = 0;
    for (const auto& param : funcSymbol->getParams()) {
        const std::string& name = param.first;
        const std::string& type = param.second;
        if(type=="const") addSymbol(name, CONSTVAR, type);
        else addSymbol(name, VAR, type);

        paramOffsets[name] = currentOffset;
        currentOffset += 4; //每个参数都是int，占4字节

    }
    return paramOffsets;
}

void SymbolTable::PrintVarSymbols(std::vector<std::string>& strs) {
    if (tableStack.empty()) {
        return; //没有全局作用域
    }

    std::set<std::string> printedSymbols;
    const auto& globalScope = tableStack[0];
    for (const auto& entry : globalScope) {
        const std::string& name = entry.first;
        std::shared_ptr<Symbol> symPtr = entry.second;

        if (symPtr->getKind() != FUNC) {
            if (printedSymbols.count(name)) {
                continue;
            }
            int initialValue = 0; //默认初始值为0
            if (symPtr->hasInitialValue()) {
                initialValue = symPtr->getInitialValue();
            }
            std::string str= name+": .word "+std::to_string(initialValue);
            strs.emplace_back(str);
            printedSymbols.insert(name);
        }
    }
}

/*四元式代码生成和输出*/
int IR::tempCount = 0;
int IR::labelCount = 0;
bool IR::isT = 1;
std::vector<Quadruple> IR::IRlist;

std::string IR::newTemp() {
    return "$t" + std::to_string(tempCount++ % 10);
}//生成新的临时变量名，用于在表达式求值中，为中间结果分配临时变量

void IR::emit(const std::string& op, const std::string& arg1,
    const std::string arg2, const std::string result) {
    if (!isFunction) {
        IRlist.emplace_back(op, arg1, arg2, result);
    }
    else if (func!=NULL) {
        Quadruple quad(op, arg1, arg2, result);
        func->setFunctionIR(quad);
    }
    
}

const std::vector<Quadruple>& IR::getCode() {
    return IRlist;
}

void IR::dumpToFile(std::ostream& out) {
    for (auto& q : IRlist) {
        q.printIR(out);
    }
}

std::string IR::newLabel() {
    return "L" + std::to_string(labelCount++);
} //函数标签

/*翻译器部分*/

std::string Translator::getReg(const std::string& var) {
    if (regMap.count(var)) return regMap[var];
    std::string reg = "$t" + std::to_string(tempCount++ % 10);
    regMap[var] = reg;
    return reg;
}

void Translator::emit(const std::string& code) {
    std::string str = "    " + code;
    strMIPS_1.emplace_back(str);
}

void Translator::printMIPS() {
    for (auto str : strMIPS_3) {
        out << str << "\n";
    }
}

void Translator::insertDataMIPS() {
    std::string beginstr= ".data";
    strMIPS_2.emplace_back(beginstr);
    strPool.emit(strMIPS_2);
}

void Translator::MainGenMIPS() {
    //这里放函数
    genAllFunctions();

    strMIPS_1.emplace_back("main:");
    genMIPS(ir);
    //生成输出串strMIPS_1

    insertDataMIPS(); //生成字符表strMIPS_2

    //合并
    strMIPS_3 = strMIPS_2;
    //这里放变量
    symTable.PrintVarSymbols(strMIPS_3);
    strMIPS_3.emplace_back(".text\n.globl main");
    strMIPS_3.insert(strMIPS_3.end(), strMIPS_1.begin(), strMIPS_1.end());

    //打印
    for (auto str : strMIPS_3) {
        out << str << std::endl;
    }


    //这个是main函数终止
    emit("li $v0, 10");
    emit("syscall");
}





void Translator::genAllFunctions() {
    std::set<std::string> generated;

    for (int level = symTable.currentLevel; level >= 0 && level < symTable.tableStack.size(); --level) {
        for (const auto& entry : symTable.tableStack[level]) {
            const std::string& name = entry.first;
            std::shared_ptr<Symbol> sym = entry.second;

            if (sym->getKind() != FUNC || generated.count(name)) {
                continue;
            }
            strMIPS_1.emplace_back(name+":");
            genMIPS(sym->getfunctionIRs());
            emit("jr $ra");
            generated.insert(name);
        }
    }
}


void Translator::genMIPS(std::vector<Quadruple> IRs) {
    for (auto q : IRs) {
        const std::string op = q.op;
        const std::string a1 = q.arg1;
        const std::string a2 = q.arg2;
        const std::string res = q.result;

        if (op == ":=") {
            std::string srcOperandNameInIR = q.arg1;
            std::string destOperandNameInIR = q.result;

            std::string srcMipsReg = prepareOperandRegister(srcOperandNameInIR, "_src_assign");
            std::shared_ptr<Symbol> destSym = symTable.lookup(destOperandNameInIR);
            if (destSym) {
                //全局变量
                if (destSym->getLevel() == 0) {
                    emit("sw " + srcMipsReg + ", " + destOperandNameInIR);
                }
                else {
                    //不是全局变量，要移动到MIPS寄存器
                    std::string destMipsReg = getReg(destOperandNameInIR);
                    if (destMipsReg != srcMipsReg) {
                        emit("move " + destMipsReg + ", " + srcMipsReg);
                    }
                }
            }
            else {
                reportSemanticError("Target operand " + destOperandNameInIR + " not found for assignment.");
            }
        }
        /*------------算数运算--------------*/
        else if (op == "+") {
            std::string reg_a1 = prepareOperandRegister(a1, "_op1"); // a1 是IR中的第一个操作数名
            std::string reg_a2 = prepareOperandRegister(a2, "_op2"); // a2 是IR中的第二个操作数名
            std::string reg_res = getReg(res); // res 是IR中的结果名, getReg会为它分配MIPS寄存器
            emit("addu " + reg_res + ", " + reg_a1 + ", " + reg_a2);
        }
        else if (op == "-") {
            std::string reg_a1 = prepareOperandRegister(a1, "_sub_a1");
            std::string reg_a2 = prepareOperandRegister(a2, "_sub_a2");
            std::string reg_res = getReg(res);
            emit("subu " + reg_res + ", " + reg_a1 + ", " + reg_a2); // 使用 subu        }
        }
        else if (op == "*") {
            std::string reg_a1 = prepareOperandRegister(a1, "_mul_a1");
            std::string reg_a2 = prepareOperandRegister(a2, "_mul_a2");
            std::string reg_res = getReg(res);
            // MIPS 乘法结果在 HI/LO 寄存器
            emit("mult " + reg_a1 + ", " + reg_a2);
            emit("mflo " + reg_res); // 只取低32位结果
        }
        else if (op == "/") {
            std::string reg_a1 = prepareOperandRegister(a1, "_div_a1");
            std::string reg_a2 = prepareOperandRegister(a2, "_div_a2");
            std::string reg_res = getReg(res);
            emit("div " + reg_a1 + ", " + reg_a2); // 商在LO，余数在HI
            emit("mflo " + reg_res); // 取商
        }
        else if (op == "neg") {
            std::string reg_a1 = prepareOperandRegister(a1, "_neg_a1");
            std::string reg_res = getReg(res);
            emit("subu " + reg_res + ", $zero, " + reg_a1);
        }
        else if (op == "%") {
            std::string reg_a1 = prepareOperandRegister(a1, "_mod_a1");
            std::string reg_a2 = prepareOperandRegister(a2, "_mod_a2");
            std::string reg_res = getReg(res);
            emit("div " + reg_a1 + ", " + reg_a2);
            emit("mfhi " + reg_res); // 取余数
        }
        /*-------------逻辑运算----------------*/

        else if (op == "!") {
            std::string reg_a1 = prepareOperandRegister(a1, "_not_a1");
            std::string reg_res = getReg(res);
            // result = (arg1 == 0) ? 1 : 0
            emit("seq " + reg_res + ", " + reg_a1 + ", $zero");
        }
        else if (op == "AND") {
            std::string reg_a1 = prepareOperandRegister(a1, "_and_a1");
            std::string reg_a2 = prepareOperandRegister(a2, "_and_a2");
            std::string reg_res = getReg(res);
            emit("and " + reg_res + ", " + reg_a1 + ", " + reg_a2);
        }
        else if (op == "OR") {
            std::string reg_a1 = prepareOperandRegister(a1, "_or_a1");
            std::string reg_a2 = prepareOperandRegister(a2, "_or_a2");
            std::string reg_res = getReg(res);
            emit("or " + reg_res + ", " + reg_a1 + ", " + reg_a2);
        }
        /*----------比较运算------------*/         
        else if (op == "==") {
            std::string reg_a1 = prepareOperandRegister(a1, "_eql_a1");
            std::string reg_a2 = prepareOperandRegister(a2, "_eql_a2");
            std::string reg_res = getReg(res);
            emit("seq " + reg_res + ", " + reg_a1 + ", " + reg_a2);
        }
        else if (op == "<") {
            std::string reg_a1 = prepareOperandRegister(a1, "_lss_a1");
            std::string reg_a2 = prepareOperandRegister(a2, "_lss_a2");
            std::string reg_res = getReg(res);
            emit("slt " + reg_res + ", " + reg_a1 + ", " + reg_a2);
        }
        else if (op == "<=") {
            std::string reg_a1 = prepareOperandRegister(a1, "_leq_a1");
            std::string reg_a2 = prepareOperandRegister(a2, "_leq_a2");
            std::string reg_res = getReg(res);
            emit("sle " + reg_res + ", " + reg_a1 + ", " + reg_a2);
            emit("sgt " + reg_res + ", " + reg_a1 + ", " + reg_a2); // reg_res = (a1 > a2)
            emit("xori " + reg_res + ", " + reg_res + ", 1");
        }
        else if (op == ">") {
            std::string reg_a1 = prepareOperandRegister(a1, "_gre_a1");
            std::string reg_a2 = prepareOperandRegister(a2, "_gre_a2");
            std::string reg_res = getReg(res);
            emit("sgt " + reg_res + ", " + reg_a1 + ", " + reg_a2);
        }
        else if (op == ">=") {
            std::string reg_a1 = prepareOperandRegister(a1, "_geq_a1");
            std::string reg_a2 = prepareOperandRegister(a2, "_geq_a2");
            std::string reg_res = getReg(res);
            // Breakdown: !(a1 < a2)
            emit("slt " + reg_res + ", " + reg_a1 + ", " + reg_a2); // reg_res = (a1 < a2)
            emit("xori " + reg_res + ", " + reg_res + ", 1");     // reg_res = !(a1 < a2) which is a1 >= a2
        }
        else if (op == "!=") {
            std::string reg_a1 = prepareOperandRegister(a1, "_neq_a1");
            std::string reg_a2 = prepareOperandRegister(a2, "_neq_a2");
            std::string reg_res = getReg(res);
            emit("sne " + reg_res + ", " + reg_a1 + ", " + reg_a2);
        }
        /*------------函数调用-----------------*/
        else if (op == "param") {
            //函数前准备与重置没写
            if (currentArgRegIdx < 4) {
                //参数少于4个，不用压栈
                std::string paramValReg = prepareOperandRegister(a1, "_param_val");
                emit("move $a" + std::to_string(currentArgRegIdx) + ", " + paramValReg);
            }
            else {
                //参数多于4个，要压栈
                std::string paramValReg = prepareOperandRegister(a1, "_param_stack_val");
                emit("addiu $sp, $sp, -4"); // 为参数在栈上分配空间
                emit("sw " + paramValReg + ", 0($sp)"); // 将参数值压栈
            }
            currentArgRegIdx++;
        }
        else if (op == "CALL") {
            currentArgRegIdx = 0;
            //需要处理需要保存的寄存器，但是先不处理
            emit("jal " + a1); // 跳转并链接到函数标签 a1
            currentFunctionName = a1;
            // 调用者清理栈上传递的参数 (如果参数 > 4)
            int numParamsOnStack = std::max(0, currentArgRegIdx - 4);
            if (numParamsOnStack > 0) {
                emit("addiu $sp, $sp, " + std::to_string(numParamsOnStack * 4));
            }
            currentArgRegIdx = 0; // 重置参数计数器以备下次调用

            if (res != "_" && !res.empty()) { // 如果函数有返回值且需要保存它
                std::string reg_res = getReg(res);
                emit("move " + reg_res + ", $v0"); // 返回值在 $v0
            }
        }
        else if (op == "RETURN") {
            if (a1 != "_" && !a1.empty()) {
                std::string retValReg = prepareOperandRegister(a1, "_ret_val");
                emit("move $v0, " + retValReg); // 将返回值放入 $v0
            }
            emit("b " + currentFunctionName + "_epilogue");
        }
        /*------------输入和输出--------------*/
        else if (op == "READ") {
            emit("li $v0, 5"); // syscall code for read_integer
            emit("syscall");
            std::string reg_res = getReg(res);
            emit("move " + reg_res + ", $v0"); // 读取到的整数在 $v0
        }
        else if (op == "WRITE") {
            std::string label;
            bool isStringLabel = false;
            //在字符串池中查找
            if (!a1.empty()&&strPool.search(a1)) {
                label = strPool.getLabel(a1);
                isStringLabel = true;
            }
            else {
                //字符串池没有这个，那就添加
                if (!a1.empty() && a1.back() == '"' && a1.front() == '"') {
                    strPool.addString(a1);
                    isStringLabel = true;
                }
            }
            if (isStringLabel) { // 假设 a1 就是字符串的标签
                emit("li $v0, 4"); // syscall code for print_string
                emit("la $a0, " + label); // 加载字符串标签的地址
                emit("syscall");
            }
            else { // 打印整数值
                std::string valToPrintReg = prepareOperandRegister(a1, "_write_val");
                emit("li $v0, 1"); // syscall code for print_integer
                emit("move $a0, " + valToPrintReg);
                emit("syscall");
            }
        }
        else if (op == "LABEL") {
            emit(q.arg1 + ":");
        }
        else if (op == "GOTO") {
            emit("j " + q.arg1);
        }
        else if (op == "BZ") {
            std::string condMipsReg = getReg(q.arg1);
            emit("beqz " + condMipsReg + ", " + q.arg2);
        }
        else {
            emit("# UNHANDLED IR OP: " + op);
        }
    }

}

std::string Translator::prepareOperandRegister(const std::string& operandName, const std::string& tempRegPurposeSuffix) {
    //检查是否为立即数
    if (!operandName.empty()) {
        std::string numericValue;
        bool isNumericImmediate = false;

        if (operandName[0] == '$' && operandName.length() > 1 &&
            std::all_of(operandName.begin() + 1, operandName.end(), ::isdigit)) {
            numericValue = operandName.substr(1);
            isNumericImmediate = true;
        }
        else if (std::all_of(operandName.begin(), operandName.end(), ::isdigit)) {
            numericValue = operandName;
            isNumericImmediate = true;
        }

        if (isNumericImmediate) {
            std::string tempIrNameForImm="$imm_"+ numericValue + tempRegPurposeSuffix;
            std::string mipsReg = getReg(tempIrNameForImm);
            emit("li " + mipsReg + ", " + numericValue);
            return mipsReg;
        }
    }

    //检查是否为已知的符号
    std::shared_ptr<Symbol> symbol = symTable.lookup(operandName);
    if (symbol) {
        if (symbol->getKind() != FUNC && symbol->getLevel() == 0) {
            //是全局变量
            //为加载的全局变量的值分配一个IR临时名，再通过getReg映射
            std::string tempIrNameForGlobalLoad = "$load_global_" + operandName + tempRegPurposeSuffix;
            std::string mipsReg = getReg(tempIrNameForGlobalLoad);
            emit("lw " + mipsReg + ", " + operandName); // operandName 是全局变量的标签
            return mipsReg;
        }
    }
    return getReg(operandName);
}









/*============================主函数部分===========================*/

int main()
{
    std::vector<Word> wl;
    std::string fn = "testfile.txt";
    Lexer lexer(fn);
    
    std::ofstream outFile("output.txt");

    if (!outFile.is_open()) {
        std::cerr << "Cannot open the file!" << std::endl;
        return 1;
    }

    Parser parser(lexer, outFile);

    try {
        auto ast = parser.Parse();
    }
    catch (const std::exception& e) {
        std::cerr << "语法分析出错: " << e.what() << std::endl;
    }
    outFile.close();

    std::ofstream outputASMfile("mips.txt");

    // Step 4: Translate to MIPS Assembly
    const std::vector<Quadruple>& irCode = IR::getCode();
    if (!outputASMfile) {
        std::cerr << "Failed to open assembly output file." << std::endl;
        return 1;
    }

    Translator trlatr(irCode, outputASMfile);
    trlatr.MainGenMIPS();
    outputASMfile.close();

    std::cout << "Compilation successful. IR written to " << "IR.txt"
        << ", assembly to " << "mips.txt" << std::endl;

    

    return 0;
}


