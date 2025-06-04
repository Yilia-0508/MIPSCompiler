#pragma once
#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <cstdlib>
#include <cctype>
#include <unordered_set>
#include <memory>
#include <unordered_map>
#include <stack>
#include <map>
#include <set>
#include <algorithm>

#define IFDEBUG 0

enum CLASS_CODE {
    IDENFR,
    INTCON,
    STRCON,
    MAINTK,
    CONSTTK,
    INTTK,
    BREAKTK,
    CONTINUETK,
    IFTK,
    ELSETK,
    NOT,
    AND,
    OR,
    WHILETK,
    GETINTTK,
    PRINTFTK,
    RETURNTK,
    PLUS,
    MINU,
    VOIDTK,
    MULT,
    DIV,
    MOD,
    LSS,
    LEQ,
    GRE,
    GEQ,
    EQL,
    NEQ,
    ASSIGN,
    SEMICN,
    COMMA,
    LPARENT,
    RPARENT,
    LBRACK,
    RBRACK,
    LBRACE,
    RBRACE,
    END,
    UNKNOWN
};

static std::string ClassCodeToString(CLASS_CODE c);
void reportSemanticError(const std::string& msg);

class Word {
public:
    Word() {}
    Word(std::string w, CLASS_CODE c) :word(w), code(c) {}
    void setcode(CLASS_CODE c) { this->code = c; }
    void setword(std::string s) { this->word = s; }
    CLASS_CODE getcode() { return this->code; }
    std::string getword() { return this->word; }
    void Print(std::ofstream& fout) {
        if (code == STRCON) {
            fout << ClassCodeToString(code) << " " << "\"" << word << "\"" << std::endl;
        }
        else fout << ClassCodeToString(code) << " " << word << std::endl;
    }
    Word operator=(const Word& w) {
        word = w.word;
        code = w.code;
        return *this;
    }

private:
    std::string word;
    CLASS_CODE code;
};


/*=================字符串池====================*/


class StringPool {
public:
    std::string addString(const std::string& str);
    void emit(std::vector<std::string>& strs);
    bool search(std::string s);
    std::string getLabel(std::string s);

private:
    std::unordered_map<std::string, std::string> strToLabel;
    int labelCounter = 0;

    std::string escape(const std::string& s) {
        std::string result;
        for (char c : s) {
            if (c == '\n') {
                result += "\\n";
            }
            else result += c;
        }
        return result;
    }

};


/*==============语法分析器===============*/


class Lexer {
public:
    Lexer(const std::string& fn);
    void ReadFile();
    Word GetNextToken();
    Word PeekNextToken();
    Word Peek2ndToken();

private:
    std::string input;
    size_t pos;
    char c;
    size_t length;
    std::string filename;

    bool nextTokenCached;
    Word nextToken;
    bool secondTokenCached;
    Word secondToken;

    Word NextWord();
    void Advance();
    void SkipSpace();
    void SkipComment();
    Word Identifier();
    Word Number();
    Word Op();
    Word Str();

};

/*===========================符号表部分==================================*/
enum SymbolKind { VAR, FUNC, CONSTVAR };
class Quadruple {
public:
    Quadruple(std::string op, std::string arg1, std::string arg2, std::string result) :op(op), arg1(arg1), arg2(arg2), result(result) {}
    void printIR(std::ostream& out) {
        out << "("
            << op << ", "
            << arg1 << ", "
            << arg2 << ", "
            << result << ")" << std::endl;
    }
    friend class Translator; //友元类

private:
    std::string op;
    std::string arg1;
    std::string arg2;
    std::string result;
};


class Symbol {
private:
    std::string name;
    SymbolKind kind; //类型：变量/常量/函数
    std::string type; //数据类型：int/void
    int level; //作用域层级
    int address; //内存偏移或栈偏移位置
    bool isDefined;

    int value=0; //变量值

    std::vector<std::pair<std::string, std::string>> params; //函数，参数名+类型
    std::vector<Quadruple> functionIRs;
    

public:
    Symbol() = default;
    Symbol(const std::string n, SymbolKind sk, const std::string t, int l, int a)
        : name(n), kind(sk), type(t), level(l), address(a), isDefined(true) {}

    Symbol& operator=(const Symbol& other);
    void paramCopy(std::vector<std::pair<std::string, std::string>> p);
    const std::vector<std::pair<std::string, std::string>>& getParams() const;
    SymbolKind getKind() {
        return kind;
    }
    void setValue(int v) {
        value = v;
    }

    void emitFuncIR(std::string op, std::string arg1, std::string arg2, std::string result) {
        functionIRs.push_back(Quadruple(op, arg1, arg2, result));
    }

    std::vector<Quadruple>& getfunctionIRs() {
        return functionIRs;
    }

    void setFunctionIR(Quadruple ir) {
        functionIRs.emplace_back(ir);
    }

    int getLevel() const { return level; }

    bool hasInitialValue() {
        return isDefined;
    }

    int getInitialValue() {
        return value;
    }

};

class Translator;


//符号表管理类
class SymbolTable {
public:
    std::vector<std::unordered_map<std::string, std::shared_ptr<Symbol>>> tableStack;
    int currentLevel;
    std::stack<int> offsetStack;

    SymbolTable();
    void enterScope();
    void exitScope();
    bool addSymbol(const std::string& name, SymbolKind kind, const std::string& type);
    std::shared_ptr<Symbol> lookup(const std::string& name);
    bool addFunction(const std::string& name, const std::string& returnType,
        const std::vector<std::pair<std::string, std::string>>& paramList,
        std::unordered_map<std::string, int>& outParamOffsets);
    std::unordered_map<std::string, int> enterFunctionBody(std::shared_ptr<Symbol> funcSymbol);
    void PrintVarSymbols(std::vector<std::string>& strs);
};

class ASTNode;
class PrintfNode;

//插入四元式代码部分
void generateIRForPrintf(const std::unique_ptr<ASTNode>& node);


//四元式表示和中间代码生成

class IR {
public:
    static std::string newTemp();
    static void emit(const std::string& op, const std::string& arg1,
        const std::string arg2, const std::string result);
    static const std::vector<Quadruple>& getCode();
    static void dumpToFile(std::ostream& out); 

    static std::string newLabel();
    
    static bool isFunction;
    static std::shared_ptr<Symbol> func;

private:
    static std::vector<Quadruple> IRlist;
    static int tempCount;
    static int labelCount;
    static bool isT;


};


/*==============================语法分析器部分==============================*/
//基类
class ASTNode {
public:
    virtual ~ASTNode() = default;
    std::string value; //中间代码生成时的结果变量名（如t0，t1或变量名）
};

//抽象语法树

class CompUnitNode : public ASTNode {
public:
    std::vector<std::unique_ptr<ASTNode>> decls; //Decl
    std::vector<std::unique_ptr<ASTNode>> funcs; //FuncDef
    std::unique_ptr<ASTNode> mainfunc; //MainFuncDef
};

class VarDefNode : public ASTNode {
public:
    std::string varName;
    std::unique_ptr<ASTNode> initVal;
    std::unique_ptr<ASTNode> constExp; //nullptr说明不是数组
    VarDefNode(std::string n, std::unique_ptr<ASTNode> c, std::unique_ptr<ASTNode> i)
        :varName(n), constExp(std::move(c)), initVal(std::move(i)) {
        this->value = varName;
    }
};

class VarDeclNode : public ASTNode {
public:
    std::vector<std::unique_ptr<ASTNode>> varDefs; //变量初值
    VarDeclNode(std::vector<std::unique_ptr<ASTNode>> defs)
        :varDefs(std::move(defs)) {}
};

class BTypeNode : public ASTNode {
public:
    std::string type;
    BTypeNode(std::string t) :type(t) {}
}; //只有int一种

class ConstDefNode : public ASTNode {
public:
    std::string varName;
    std::unique_ptr<ASTNode> initVal;
    std::unique_ptr<ASTNode> constExp; //optional, nullptr表示不是数组
    ConstDefNode(std::string n, std::unique_ptr<ASTNode> c, std::unique_ptr<ASTNode> i)
        :varName(n), constExp(std::move(c)), initVal(std::move(i)) {
        this->value = varName;
    }
};

class ConstDeclNode : public ASTNode {
public:
    std::unique_ptr<ASTNode> btype;
    std::vector<std::unique_ptr<ASTNode>> constdefs;
    ConstDeclNode(std::unique_ptr<ASTNode> b = nullptr)
        :btype(std::move(b)) {} //constdefs后面再加
};

class ConstInitValNode : public ASTNode {
public:
    std::vector<std::unique_ptr<ASTNode>> values; //标量只有一个值，数组有整个数组元素
    ConstInitValNode(std::vector<std::unique_ptr<ASTNode>> vals)
        : values(std::move(vals)) {}
};

class InitValNode : public ASTNode {
public:
    std::unique_ptr<ASTNode> singleVal; //单个表达式初始化
    std::vector<std::unique_ptr<ASTNode>> multiVals; //数组初始化
    InitValNode(std::unique_ptr<ASTNode> expr)
        :singleVal(std::move(expr)) {}
    InitValNode(std::vector<std::unique_ptr<ASTNode>> exprs)
        :multiVals(std::move(exprs)) {}
};

class AddExpNode : public ASTNode {
public:
    std::unique_ptr<ASTNode> left; //左操作数
    std::string op; //操作符
    std::unique_ptr<ASTNode> right; //右操作符

    AddExpNode(std::unique_ptr<ASTNode> left, const std::string& op, std::unique_ptr<ASTNode> right)
        :left(std::move(left)), op(op), right(std::move(right)) {}
};

class MulExpNode : public ASTNode {
public:
    std::unique_ptr<ASTNode> left;
    std::string op;
    std::unique_ptr<ASTNode> right;

    MulExpNode(std::unique_ptr<ASTNode> left, const std::string& op, std::unique_ptr<ASTNode> right)
        :left(std::move(left)), op(op), right(std::move(right)) {}

};

class PrimaryExpNode : public ASTNode {
public:
    std::unique_ptr<ASTNode> expr;
    PrimaryExpNode(std::unique_ptr<ASTNode> expr)
        :expr(std::move(expr)) {}
};

class LValNode : public ASTNode {
public:
    std::string name;
    std::unique_ptr<ASTNode> indexExpr; //nullptr表示普通变量
    LValNode(std::string name, std::unique_ptr<ASTNode> idx = nullptr)
        :name(std::move(name)), indexExpr(std::move(idx)) {}
    std::string getName() { return name; }
};

class RelExpNode : public ASTNode {
public:
    std::string op;
    std::unique_ptr<ASTNode> left;
    std::unique_ptr<ASTNode> right;
    RelExpNode(std::string oper, std::unique_ptr<ASTNode> lhs, std::unique_ptr<ASTNode> rhs)
        :op(std::move(oper)), left(std::move(lhs)), right(std::move(rhs)) {}
};

class EqExpNode : public ASTNode {
public:
    std::string op;
    std::unique_ptr<ASTNode> left;
    std::unique_ptr<ASTNode> right;
    EqExpNode(std::string oper, std::unique_ptr<ASTNode> lhs, std::unique_ptr<ASTNode> rhs)
        :op(std::move(oper)), left(std::move(lhs)), right(std::move(rhs)) {}
};


class LAndExpNode : public ASTNode {
public:
    std::unique_ptr<ASTNode> left;
    std::unique_ptr<ASTNode> right;
    LAndExpNode(std::unique_ptr<ASTNode> lhs, std::unique_ptr<ASTNode> rhs)
        :left(std::move(lhs)), right(std::move(rhs)) {}
};

class LOrExpNode : public ASTNode {
public:
    std::unique_ptr<ASTNode> left;
    std::unique_ptr<ASTNode> right;
    LOrExpNode(std::unique_ptr<ASTNode> lhs, std::unique_ptr<ASTNode> rhs)
        :left(std::move(lhs)), right(std::move(rhs)) {}
};


class NumberNode : public ASTNode {
public:
    int value;
    NumberNode(int val) : value(val) {}
};//跟BType类似，虽然只有一个但一定要有

class FuncRParamsNode : public ASTNode {
public:
    std::vector<std::unique_ptr<ASTNode>> args;
    void addArg(std::unique_ptr<ASTNode> arg) {
        args.push_back(std::move(arg));
    }
};


class FuncCallNode : public ASTNode {
public:
    std::string funcName;
    std::unique_ptr<ASTNode> params;
    FuncCallNode(const std::string& name, std::unique_ptr<ASTNode> params)
        : funcName(name), params(std::move(params)) {}
    FuncCallNode(const std::string& name)
        : funcName(name) {}

    void addParams(std::unique_ptr<ASTNode> params) {
        params = std::move(params);
    }

    int paramNum() {
        if (auto* paList = dynamic_cast<FuncRParamsNode*>(params.get())) {
            return paList->args.size();
        }
        else return 0;
    }

    std::string getName() { return funcName; }
};



class FuncFParamNode : public ASTNode {
public:
    std::unique_ptr<ASTNode> type;
    std::string name;
    FuncFParamNode(std::unique_ptr<ASTNode> type, const std::string& name)
        : type(std::move(type)), name(name) {}
    std::string getName() { return name; }

};

class FuncTypeNode : public ASTNode {
public:
    std::string type;
    FuncTypeNode(std::string t)
        :type(t) {}
    std::string getType() { return type; }
}; //有int或者void两种

//函数定义
class FuncDefNode : public ASTNode {
public:
    std::string returnType;
    std::string funcName;
    std::vector<std::unique_ptr<ASTNode>> params;
    std::unique_ptr<ASTNode> body;
    FuncDefNode(std::string retType, std::string n, std::vector<std::unique_ptr<ASTNode>> pl, std::unique_ptr<ASTNode> b)
        :returnType(retType), funcName(n), params(std::move(pl)), body(std::move(b)) {}
    std::string getType() { return returnType; }
    std::string getName() { return funcName; }
};

class BlockNode : public ASTNode {
public:
    std::vector<std::unique_ptr<ASTNode>> items;
    void addItem(std::unique_ptr<ASTNode> item) {
        items.push_back(std::move(item));
    }
};

class BreakNode : public ASTNode {
    //不需要任何额外信息
};

class ContinueNode : public ASTNode {
    //不需要任何额外信息
};

class ReturnNode : public ASTNode {
public:
    std::unique_ptr<ASTNode> returnExpr; // 可以为 nullptr
    ReturnNode(std::unique_ptr<ASTNode> expr) : returnExpr(std::move(expr)) {}
};

class PrintfNode : public ASTNode {
public:
    std::string formatString;
    std::vector<std::unique_ptr<ASTNode>> args; // 每一个参数都是一个表达式
    PrintfNode(const std::string& fmt, std::vector<std::unique_ptr<ASTNode>> arguments)
        : formatString(fmt), args(std::move(arguments)) {
        this->value = formatString;
    }
    std::string getForStr() { return formatString; }
};

class GetintNode : public ASTNode {
    //无需任何额外信息
};


class AssignNode : public ASTNode {
public:
    std::unique_ptr<ASTNode> lval;
    std::unique_ptr<ASTNode> expr;
    bool isFromGetInt;
    AssignNode(std::unique_ptr<ASTNode> lval,
        std::unique_ptr<ASTNode> expr = nullptr,
        bool isFromGetInt = false)
        : lval(std::move(lval)), expr(std::move(expr)), isFromGetInt(isFromGetInt) {}

};

class ExprStmtNode : public ASTNode {
public:
    std::unique_ptr<ASTNode> expr;  // 表达式可能为空（空语句）

    ExprStmtNode(std::unique_ptr<ASTNode> expr)
        : expr(std::move(expr)) {}
};

class MainFuncDefNode : public ASTNode {
public:
    std::unique_ptr<ASTNode> body; // Block 节点
    MainFuncDefNode(std::unique_ptr<ASTNode> body)
        : body(std::move(body)) {}
};

class IfNode : public ASTNode {
public:
    std::unique_ptr<ASTNode> condition;
    std::unique_ptr<ASTNode> thenStmt;
    std::unique_ptr<ASTNode> elseStmt; // 可以为 nullptr

    IfNode(std::unique_ptr<ASTNode> cond,
        std::unique_ptr<ASTNode> thenBranch,
        std::unique_ptr<ASTNode> elseBranch = nullptr)
        : condition(std::move(cond)),
        thenStmt(std::move(thenBranch)),
        elseStmt(std::move(elseBranch)) {}
};

class WhileNode : public ASTNode {
public:
    std::unique_ptr<ASTNode> condition;
    std::unique_ptr<ASTNode> body;

    WhileNode(std::unique_ptr<ASTNode> cond,
        std::unique_ptr<ASTNode> bodyStmt)
        : condition(std::move(cond)),
        body(std::move(bodyStmt)) {}
};

//语法分析器

class Parser {
public:
    Parser(Lexer& l, std::ofstream& fout);
    std::unique_ptr<ASTNode> Parse();

private:
    Lexer& lexer;
    Word currentToken;
    std::ofstream& fout;

    std::string curBType;
    std::string curFuncType;

    void PrintSymbol(const std::string& symbol);
    void PrintToken(Word& word);
    void Advance();
    std::unique_ptr<ASTNode> ParseDecl(bool ifFunc = 0);
    std::unique_ptr<ASTNode> ParseBType();
    std::unique_ptr<ASTNode> ParseConstDecl();
    std::unique_ptr<ASTNode> ParseConstDef();
    std::unique_ptr<ASTNode> ParseConstInitVal();
    std::unique_ptr<ASTNode> ParseVarDecl();
    std::unique_ptr<ASTNode> ParseVarDef();
    std::unique_ptr<ASTNode> ParseInitVal();
    std::unique_ptr<ASTNode> ParseAddExp();
    std::unique_ptr<ASTNode> ParseMulExp();
    std::unique_ptr<ASTNode> ParseUnaryExp();
    std::unique_ptr<ASTNode> ParsePrimaryExp();
    std::unique_ptr<ASTNode> ParseLVal(bool ifFunc = 0);
    std::unique_ptr<ASTNode> ParseExp(bool ifFunc = 0);
    std::unique_ptr<ASTNode> ParseRelExp();
    std::unique_ptr<ASTNode> ParseConstExp();
    std::unique_ptr<ASTNode> ParseFuncRParams();
    std::unique_ptr<ASTNode> ParseEqExp();
    std::unique_ptr<ASTNode> ParseLAndExp();
    std::unique_ptr<ASTNode> ParseLOrExp(bool ifFunc = 0);
    std::unique_ptr<ASTNode> ParseFuncCall();
    std::unique_ptr<ASTNode> ParseFuncDef();
    std::string ParseFuncType();
    std::vector<std::unique_ptr<ASTNode>> ParseFuncFParams();
    std::unique_ptr<ASTNode> ParseFuncFParam();
    std::unique_ptr<ASTNode> ParseBlock(bool ifFunc = 0);
    std::unique_ptr<ASTNode> ParseBlockItem(bool ifFunc = 0);
    std::unique_ptr<ASTNode> ParseStmt(bool ifFunc = 0);
    std::unique_ptr<ASTNode> ParseIfStmt(bool ifFunc = 0);
    std::unique_ptr<ASTNode> ParseCond(bool ifFunc = 0);
    std::unique_ptr<ASTNode> ParseWhileStmt(bool ifFunc = 0);
    std::unique_ptr<ASTNode> ParseCompUnit();
    std::unique_ptr<ASTNode> ParseMainFuncDef();

    void Expect(CLASS_CODE expectedcode);
};

std::string extractTypeString(const std::unique_ptr<ASTNode>& node);


/*====================生成MIPS汇编部分===========================*/

class Translator {
public:
    Translator(const std::vector<Quadruple>& ir, 
        std::ostream& out) : ir(ir),out(out) {}
    void genMIPS(std::vector<Quadruple> IRs);
    void MainGenMIPS();
    void genAllFunctions();

    void printMIPS();
    void insertDataMIPS();

private:
    const std::vector<Quadruple>& ir;
    std::ostream& out;
    std::map<std::string, std::string>regMap;
    int tempCount = 0; 
    int currentArgRegIdx = 0; 
    std::string currentFunctionName;
    std::vector<std::string> strMIPS_1;
    std::vector<std::string> strMIPS_2;
    std::vector<std::string> strMIPS_3;

    std::string getReg(const std::string& var);
    void emit(const std::string& code);
    std::string prepareOperandRegister(const std::string& operandName, const std::string& tempRegPurposeSuffix = "");
};

