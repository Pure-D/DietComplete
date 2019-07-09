module dietc.parser;

import dietc.lexer;

import std.algorithm;
import std.array : join;
import std.conv : text, to;
import std.meta : AliasSeq;

alias ASTClasses = AliasSeq!(Document, HiddenComment, Comment, DStatement,
		DietFilter, TagNode, TagNode.AttributeAST, RawAssignment,
		Assignment, StringTagContents, TextLine, XMLNode, PipeText, Expression, TextLine.PartAST);

interface AST
{
	Token token() @property;
	void accept(ASTVisitor visitor);
}

abstract class ASTVisitor
{
	static foreach (T; ASTClasses)
		void visit(T ast)
	in(ast !is null)
	{
		ast.accept(this);
	}

	void visit(AST ast)
	in(ast !is null)
	{
		static foreach (T; ASTClasses)
			if (cast(T) ast)
				return visit(cast(T) ast);
		throw new Exception("Unknown ast passed?!");
	}
}

enum VisitResult
{
	continue_,
	recurse,
	return_
}

alias VisitorDelegate = VisitResult delegate(AST node, AST parent);

void traverse(AST node, VisitorDelegate callback)
{
	static class VisitorImpl : ASTVisitor
	{
		static foreach (T; ASTClasses)
			override void visit(T ast)
			{
				if (result == VisitResult.return_)
					return;

				result = callback(ast, parents[$ - 1]);
				if (result == VisitResult.return_)
					return;

				if (result == VisitResult.recurse)
					{
					parents ~= ast;
					scope (exit)
						parents.length--;
					ast.accept(this);
				}
			}

		alias visit = ASTVisitor.visit;

		VisitResult result;
		AST[] parents;
		VisitorDelegate callback;

		this(AST node, VisitorDelegate callback)
		{
			parents = [node];
			this.callback = callback;
		}
	}

	node.accept(new VisitorImpl(node, callback));
}

class Document : AST, Node
{
	Node[] _children;
	Token _token;

	Node[] children() @property
	{
		return _children;
	}

	void addChild(Node child)
	{
		_token.range[1] = child.token.range[1];
		_children ~= child;
	}

	Token token() @property
	{
		return _token;
	}

	void accept(ASTVisitor visitor)
	{
		foreach (child; _children)
			if (child)
				visitor.visit(child);
	}

	this(Token token)
	{
		_token = token;
	}

	override string toString() const
	{
		string ret = "Document(";
		foreach (child; _children)
			ret ~= "\n" ~ child.to!string.indent;
		return ret ~= ")";
	}
}

interface Node : AST
{
	Node[] children() @property;
	void addChild(Node);
}

interface INamed
{
	string name() @property;
}

interface IStringContainer : AST
{
	string content() @property;
}

abstract class StringNode : Node, IStringContainer
{
	Token _token;
	string _content;
	Node[] _children;

	Node[] children() @property
	{
		return _children;
	}

	void addChild(Node child)
	{
		_token.range[1] = child.token.range[1];
		_children ~= child;
	}

	Token token() @property
	{
		return _token;
	}

	void accept(ASTVisitor visitor)
	{
		foreach (child; _children)
			if (child)
				visitor.visit(child);
	}

	string content() @property
	{
		return _content;
	}

	this(Token token, string content)
	{
		_token = token;
		_content = content;
	}

	override string toString()
	{
		import std.array : join;

		string ret = text('(', (cast(Object) this).classinfo.name, `) "`, content, '"');
		foreach (child; children)
			ret ~= "\n" ~ child.to!string.indent;
		return ret;
	}
}

class Comment : StringNode
{
	this(Token token, string content)
	{
		super(token, content);
	}
}

class HiddenComment : Comment
{
	this(Token token, string content)
	{
		super(token, content);
	}
}

class DStatement : StringNode
{
	this(Token token, string content)
	{
		super(token, content);
	}
}

class DietFilter : StringNode, INamed
{
	string _name;

	string name() @property
	{
		return _name;
	}

	this(Token token, string name, string content)
	{
		super(token, content);
		_name = name;
	}
}

interface NestedTags : Node, TagContents
{
}

class TagNode : NestedTags, INamed
{
	// NormalTagStart (TextBlock | NestedStart NestedTags | TagContents)
	// NormalTagStart: TAG_IDENTIFIER? ( NodeID | NodeClass )* Attributes? ( FitInside? FitOutside? | FitOutside FitInside ) Translated?
	// TagContents: RawAssignment | Assignment | ' '? TextLine | EOL

	struct Attribute
	{
		Token name;
		Expression expr;

		string toString()
		{
			return name.content ~ name.range.to!string ~ (expr ? "='" ~ expr.content ~ "'" : "×");
		}
	}

	/// Wrapper class around Attribute used for visiting
	class AttributeAST : AST, INamed
	{
		Attribute attribute;

		this(Attribute attribute)
		{
			this.attribute = attribute;
		}

		Token token() @property
		{
			auto tok = attribute.name;
			if (attribute.expr)
				tok.range[1] = attribute.expr.token.range[1];
			return tok;
		}

		string name() @property
		{
			return attribute.name.content;
		}

		void accept(ASTVisitor visitor)
		{
			if (attribute.expr)
				visitor.visit(attribute.expr);
		}

		override string toString()
		{
			return attribute.toString();
		}
	}

	Token[] directIDs, directClasses;
	Attribute[] attributes;
	Token _fitInside, _fitOutside, _translated;
	Token _token, _tag;
	TagContents _contents;
	Node[] _children;
	size_t[2] attributesRange;

	Node[] children() @property
	{
		return _children;
	}

	void accept(ASTVisitor visitor)
	{
		foreach (attr; attributes)
			visitor.visit(new AttributeAST(attr));
		if (_contents !is null)
			visitor.visit(_contents);
		foreach (child; _children)
			if (child)
				visitor.visit(child);
	}

	void addChild(Node child)
	{
		_token.range[1] = child.token.range[1];
		_children ~= child;
	}

	Token tag() @property
	{
		return _tag;
	}

	Token token() @property
	{
		return _token;
	}

	/// Returns: the tag name.
	string name() @property
	{
		return _tag.content;
	}

	/// Returns: true if the `<` whitespace modifier token is present
	bool fitInside() @property
	{
		return _fitInside != Token.init;
	}

	/// Returns: true if the `>` whitespace modifier token is present
	bool fitOutside() @property
	{
		return _fitOutside != Token.init;
	}

	/// Returns: true if the `&` token is present
	bool translated() @property
	{
		return _translated != Token.init;
	}

	/// Content of the tag (nullable)
	TagContents contents() @property
	{
		return _contents;
	}

	this(Token tag)
	{
		this(tag, null);
	}

	this(Token tag, TagContents contents)
	{
		_tag = _token = tag;
		_contents = contents;
	}

	override string toString()
	{
		import std.array : join;

		string ret = "tag" ~ token.range.to!string ~ "<" ~ _tag.range.to!string ~ name ~ ">";
		foreach (c; directClasses)
			ret ~= c.content;
		foreach (id; directIDs)
			ret ~= id.content;
		ret ~= "(";
		ret ~= attributes.to!(string[]).join(", ");
		ret ~= ")" ~ attributesRange.to!string;
		if (fitOutside)
			ret ~= ">";
		if (fitInside)
			ret ~= "<";
		if (translated)
			ret ~= "&";
		if (_contents !is null)
			ret ~= " = " ~ contents.to!string;
		foreach (child; children)
			ret ~= "\n" ~ child.to!string.indent;
		return ret;
	}
}

interface TagContents : AST
{
}

class StringTagContents : TagContents, IStringContainer
{
	Token _token;
	string _content;

	Token token() @property
	{
		return _token;
	}

	void accept(ASTVisitor)
	{
	}

	string content() @property
	{
		return _content;
	}

	this(Token token, string content)
	{
		_token = token;
		_content = content;
	}

	override string toString()
	{
		import std.array : join;

		string ret = text('(', (cast(Object) this).classinfo.name, `) "`, content, '"');
		return ret;
	}
}

class Assignment : StringTagContents
{
	this(Token token, string content)
	{
		super(token, content);
	}
}

class RawAssignment : Assignment
{
	this(Token token, string content)
	{
		super(token, content);
	}
}

class TextLine : TagContents
{
	struct Part
	{
		Token token;
		string raw;
		Expression inlineExpr;
		NestedTags inlineTag;
		bool escapeInlineExpr;

		string toString() const
		{
			string pre = token.range.to!string;
			if (raw.length)
				return pre ~ raw;
			else if (inlineExpr !is null)
				return pre ~ (escapeInlineExpr ? "#" : "!") ~ "{" ~ inlineExpr.to!string ~ "}";
			else if (inlineTag !is null)
				return pre ~ "#[" ~ inlineTag.to!string ~ "]";
			else
				return pre;
		}
	}

	/// Wrapper class around Part used for visiting
	class PartAST : AST
	{
		Part part;

		this(Part part)
		{
			this.part = part;
		}

		Token token() @property
		{
			return part.token;
		}

		void accept(ASTVisitor visitor)
		{
			if (part.inlineExpr)
				visitor.visit(part.inlineExpr);
			if (part.inlineTag)
				visitor.visit(part.inlineTag);
		}

		override string toString() const
		{
			return part.toString();
		}
	}

	Token _token;
	Part[] _parts;

	void accept(ASTVisitor visitor)
	{
		foreach (part; _parts)
			visitor.visit(new PartAST(part));
	}

	Token token() @property
	{
		return _token;
	}

	this(Token token, Part[] parts)
	{
		_token = token;
		_parts = parts;
	}

	override string toString()
	{
		string ret = _token.range.to!string ~ "'";
		foreach (part; _parts)
			ret ~= '{' ~ part.toString ~ '}';
		return ret ~ "'";
	}
}

class XMLNode : NestedTags
{
	TextLine _line;
	Node[] _children;

	Node[] children() @property
	{
		return _children;
	}

	void accept(ASTVisitor visitor)
	{
		foreach (child; _children)
			if (child)
				visitor.visit(child);
	}

	void addChild(Node child)
	{
		_line.token.range[1] = child.token.range[1];
		_children ~= child;
	}

	Token token() @property
	{
		return _line.token;
	}

	this(TextLine line)
	{
		_line = line;
	}
}

class PipeText : NestedTags
{
	Token _token;
	Token _translated;
	TagContents _content;
	Node[] _children;

	Node[] children() @property
	{
		return _children;
	}

	void accept(ASTVisitor visitor)
	{
		if (_content)
			visitor.visit(_content);
		foreach (child; _children)
			if (child)
				visitor.visit(child);
	}

	void addChild(Node child)
	{
		_token.range[1] = child.token.range[1];
		_children ~= child;
	}

	Token token() @property
	{
		return _token;
	}

	bool translated() @property
	{
		return _translated != Token.init;
	}

	TagContents content() @property
	{
		return _content;
	}

	this(Token token, Token translated, TagContents content)
	{
		_token = token;
		_translated = translated;
		_content = content;
	}
}

class Expression : AST
{
	Token _token;
	string _content;

	Token token() @property
	{
		return _token;
	}

	void accept(ASTVisitor)
	{
	}

	string content() @property
	{
		return _content;
	}

	this(Token token, string content)
	{
		_token = token;
		_content = content;
	}

	override string toString()
	{
		return _content;
	}
}

struct ASTParser
{
	DietInput input;
	Document root;

	void parseDocument()
	{
		root = new Document(input.front);
		while (true)
		{
			auto n = parseNode();
			if (!n)
				break;
			root.addChild(n);
		}
		input.expect(TokenType.eof);
	}

	Node parseNode()
	{
		auto past = input.save();
		auto val = parseNodeValue();
		if (val is null)
		{
			input = past;
			return null;
		}
		if (input.skipAll(TokenType.newline) == 0 && input.front.type != TokenType.eof)
		{
			input = past;
			return null;
		}
		if (input.peek(TokenType.indent))
		{
			input.popFront();
			while (true)
			{
				auto n = parseNode();
				if (!n)
					break;
				val.addChild(n);
			}
			input.expect(TokenType.detent);
		}
		return val;
	}

	Node parseNodeValue()
	{
		if (auto comment = parseComment())
			return comment;
		if (auto statement = parseDStatement())
			return statement;
		if (auto filter = parseFilter())
			return filter;
		if (auto nested = parseNestedTags())
			return nested;
		return null;
	}

	string parseText(bool multiline, bool allowIndent)
	{
		string ret;
		size_t indentation = 1;
		bool lastNewline;
		while (true)
		{
			auto v = input.front;
			if (lastNewline && v.type != TokenType.indent)
				break;
			if (v.type == TokenType.eof)
				break;
			if (v.type == TokenType.newline && (indentation == 1 || !multiline))
			{
				if (multiline)
				{
					lastNewline = true;
					input.popFront;
				}
				else
					break;
			}
			else
			{
				input.popFront;
				if (v.type == TokenType.indent)
				{
					if (indentation > 1 && !allowIndent)
						input.errors.error(input, v.range[0],
								"Can't indent here because it is already indented.");
					indentation++;
				}
				else if (v.type == TokenType.detent)
				{
					indentation--;
					if (indentation == 0)
						break;
				}
				else
				{
					ret ~= v.content;
				}
			}
		}
		return ret;
	}

	Comment parseComment()
	{
		auto tok = input.front;
		if (input.matchText("//"))
		{
			bool hidden = input.front.content.startsWith("-");
			string content = parseText(true, true);
			if (hidden)
				content = content[1 .. $];
			tok.type = TokenType.raw;
			tok.content = hidden ? "//-" : "//";
			tok.range[1] = input.front.range[0];
			return hidden ? new HiddenComment(tok, content) : new Comment(tok, content);
		}
		return null;
	}

	DStatement parseDStatement()
	{
		auto tok = input.front;
		if (tok.content.startsWith("-"))
		{
			string content = parseText(false, false);
			content = content[1 .. $];
			tok.content = "-";
			tok.type = TokenType.raw;
			tok.range[1] = input.front.range[0];
			return new DStatement(tok, content);
		}
		return null;
	}

	DietFilter parseFilter()
	{
		auto startTok = input.front;
		if (input.matchText(":"))
		{
			auto tok = input.front;
			string name;
			if (input.expect(TokenType.identifier))
			{
				name = tok.content;
				if (!name.validateIdentifierAlpha)
					input.errors.expect(input, tok.range[0], "identifier of type [-_a-zA-Z][-_0-9a-zA-Z]*");
			}
			input.match(TokenType.whitespace);
			string text = parseText(true, true);
			return new DietFilter(startTok, name, text);
		}
		return null;
	}

	NestedTags parseNestedTags()
	{
		if (auto comment = parseDoctype())
			return comment;
		if (auto statement = parseXML())
			return statement;
		if (auto filter = parsePipeText())
			return filter;
		if (auto nested = parseTag(true))
			return nested;
		return null;
	}

	NestedTags parseSingleTag()
	{
		if (auto comment = parseDoctype())
			return comment;
		if (auto statement = parseXML())
			return statement;
		if (auto filter = parsePipeText())
			return filter;
		if (auto nested = parseTag(false))
			return nested;
		return null;
	}

	TextLine parseTextLine()
	{
		DietInput fastForward(string inc, string dec)
		{
			auto c = input.save;
			int depth = 0;
			do
			{
				if (c.front.content == inc)
					depth++;
				else if (c.front.content == dec)
					depth--;
				c.popFront;
			}
			while (depth > 0 && c.front.type != TokenType.eof);
			return c;
		}

		Token tok = input.front;
		TextLine.Part[] parts;
		string raw;
		size_t[2] rawRange = input.index;
		immutable size_t startIndex = input.front.range[0];

		void flushRaw()
		{
			if (raw.length)
				parts ~= TextLine.Part(Token(TokenType.code, raw, rawRange), raw);
			raw = "";
		}

		while (!input.front.type.among!(TokenType.eof, TokenType.newline))
		{
			auto v = input.front;
			input.popFront;
			if (v.content == "\\")
			{
				v = input.front;
				input.popFront;
				if (!v.content.among!("!", "\\", "#"))
					input.errors.expect(input, v.range[0], "escaped !, \\ or #");
				if (!raw.length)
					rawRange[0] = v.range[0];
				raw ~= v.content;
				rawRange[1] = input.index;
			}
			else if (v.content == "!")
			{
				if (input.front.content == "{")
				{
					flushRaw();
					auto load = fastForward("{", "}");
					input.code.length = load.index;
					auto start = v.range[0];
					input.popFront();
					auto expr = parseExpression;
					auto end = input.index;
					v.range = [start, end];
					parts ~= TextLine.Part(v, null, expr, null, true);
					input = load;
				}
				else
				{
					if (!raw.length)
						rawRange[0] = v.range[0];
					raw ~= v.content;
					rawRange[1] = input.index;
				}
			}
			else if (v.content == "#")
			{
				if (input.front.content == "{")
				{
					flushRaw();
					auto load = fastForward("{", "}");
					input.code.length = load.index;
					auto start = v.range[0];
					input.popFront();
					auto expr = parseExpression;
					auto end = input.index;
					v.range = [start, end];
					parts ~= TextLine.Part(v, null, expr, null, true);
					input = load;
				}
				else if (input.front.content == "[")
				{
					flushRaw();
					auto load = fastForward("[", "]");
					input.code.length = load.index;
					auto start = v.range[0];
					input.popFront();
					auto tag = parseSingleTag;
					auto end = input.index;
					v.range = [start, end];
					parts ~= TextLine.Part(v, null, null, tag);
					input = load;
				}
				else
				{
					if (!raw.length)
						rawRange[0] = v.range[0];
					raw ~= v.content;
					rawRange[1] = input.index;
				}
			}
			else if (v.type != TokenType.newline)
			{
				if (!raw.length)
					rawRange[0] = v.range[0];
				raw ~= v.content;
				rawRange[1] = input.index;
			}
			else
				break;
			tok.range[1] = input.index;
		}
		flushRaw();
		if (!parts.length && startIndex == input.front.range[0])
			return null;
		return new TextLine(tok, parts);
	}

	TagNode parseDoctype()
	{
		auto tok = input.front;
		if (input.matchText("!!!"))
		{
			tok.content = "doctype";
			auto text = parseTextLine();
			return new TagNode(tok, text);
		}
		return null;
	}

	XMLNode parseXML()
	{
		if (input.front.content == "<")
		{
			auto v = parseTextLine();
			return new XMLNode(v);
		}
		return null;
	}

	PipeText parsePipeText()
	{
		auto tok = input.front;
		if (tok.content == "|")
		{
			input.popFront;
			Token translated;
			if (input.front.content == "&")
			{
				translated = input.front;
				input.popFront;
			}
			return new PipeText(tok, translated, parseTagContents());
		}
		return null;
	}

	Token parseAttributeIdentifier()
	{
		import std.array : array;
		import std.range : retro, chain;

		Token tok;
		tok.type = TokenType.code;
		tok.range = input.front.range;
		string ret;
		char[] stack;
		Loop: while (!input.empty)
		{
			auto front = input.front;
			if (front.type != TokenType.identifier)
			{
				switch (front.content)
				{
				case ",":
				case "=":
					if (stack.length == 0)
						break Loop;
					break;
				case ")":
				case "]":
				case "}":
					if (stack.length && stack[$ - 1] != front.content[0])
					{
						input.errors.expect(input, front.range[0],
								"'" ~ (cast(char[])(cast(ubyte[]) stack).retro.chain.array).idup ~ "' before ')'");
						stack.length = 0;
					}
					if (stack.length == 0)
						break Loop;
					stack.length--;
					break;
				case "(":
					stack ~= ')';
					break;
				case "[":
					stack ~= ']';
					break;
				case "{":
					stack ~= '}';
					break;
				case "\"":
					if (stack.length && stack[$ - 1] == '"')
					{
						stack.length--;
					}
					else
					{
						stack ~= '"';
					}
					break;
				default:
					break;
				}
			}
			ret ~= front.content;
			input.popFront;
		}
		tok.range[1] = input.front.range[0];
		tok.content = ret;
		return tok;
	}

	Expression parseExpression()
	{
		import std.array : array;
		import std.range : retro, chain;

		auto save = input.save;
		auto tok = input.front;
		string ret;
		char[] stack;
		bool escape = false;
		int level = 0;
		Loop: while (!input.empty)
		{
			auto front = input.front;
			if (front.type == TokenType.indent)
				level++;
			else if (front.type == TokenType.detent)
				level--;
			if (level < 0)
			{
				input = save;
				return null;
			}
			if (front.type == TokenType.raw)
			{
				if (stack.length && stack[$ - 1].among!('"', '\''))
				{
					if (escape)
					{
						escape = false;
					}
					else
					{
						if (front.content == "\\")
							escape = true;
						else if (front.content.length == 1 && front.content[0] == stack[$ - 1])
							stack.length--;
					}
				}
				else
				{
					switch (front.content)
					{
					case ",":
						if (stack.length == 0)
							break Loop;
						break;
					case ")":
					case "]":
					case "}":
						if (stack.length && stack[$ - 1] != front.content[0])
						{
							input.errors.expect(input, front.range[0],
									"'" ~ (cast(char[])(cast(ubyte[]) stack).retro.chain.array).idup ~ "' before ')'");
							stack.length = 0;
						}
						if (stack.length == 0)
							break Loop;
						stack.length--;
						break;
					case "(":
						stack ~= ')';
						break;
					case "[":
						stack ~= ']';
						break;
					case "{":
						stack ~= '}';
						break;
					case "\"":
						stack ~= '"';
						break;
					case "'":
						stack ~= '\'';
						break;
					default:
						break;
					}
				}
			}
			else
				escape = false;
			ret ~= front.content;
			input.popFront;
		}
		if (stack.length)
			input.errors.expect(input, input.front.range[0],
					"'" ~ (cast(char[])(cast(ubyte[]) stack).retro.chain.array).idup ~ "' before ')'");
		tok.range[1] = input.front.range[0];
		tok.content = input.code[tok.range[0] .. tok.range[1]];
		return new Expression(tok, ret);
	}

	TagNode parseTag(bool allowNested = true)
	{
		auto save = input.save;

		auto tok = input.front;

		Token[] classes, ids;

		bool parseClassOrID()
		{
			Token combineIdentifier()
			{
				Token start = input.front;
				input.popFront;
				auto next = input.front;
				if (input.expect(TokenType.identifier))
				{
					if (!validateIdentifier(next.content))
						input.errors.expect(input, next.range[0], "identifier of type [-_0-9a-zA-Z]+");
					start.content ~= next.content;
					start.range[1] = next.range[1];
				}
				return start;
			}

			if (input.front.content == ".")
			{
				classes ~= combineIdentifier();
				return true;
			}
			else if (input.front.content == "#")
			{
				ids ~= combineIdentifier();
				return true;
			}
			else
				return false;
		}

		Token tag;
		bool match;
		if (tok.content == "." || tok.content == "#")
		{
			tag = tok;
			tag.range[1] = tag.range[0];
			tag.type = TokenType.identifier;
			tag.content = "div";
			while (parseClassOrID())
			{
			}
			match = classes.length > 0 || ids.length > 0;
		}
		else if (tok.type == TokenType.identifier)
		{
			input.popFront;
			tag = tok;
			if (!tok.content.validateTagIdentifier)
				input.errors.expect(input, tok.range[0], "identifier of type [-:_0-9a-zA-Z]+");
			while (parseClassOrID())
			{
			}
			match = true;
		}

		if (match)
		{
			auto ret = new TagNode(tag);
			ret.directIDs = ids;
			ret.directClasses = classes;
			ret.attributesRange[] = input.front.range[0];
			if (input.matchText("("))
			{
				ret.attributesRange[0]++;
				ret.attributesRange[1]++;
				auto lastValid = input.save;
				// don't store in lastValid after detent happened
				bool detented = false;
				TagNode.Attribute[] lastNonDetented;
				bool lastNonDetentedHadValue;
				if (input.peek(TokenType.identifier))
				{
					lastValid = input.save;
					bool errored;
					while (input.front.content != ")")
					{
						input.skipAllWhiteGetDetent(detented);
						const wasDetented = detented;
						auto identifier = parseAttributeIdentifier();
						Expression value;
						bool validKey;
						if (!identifier.content.length)
							errored = true;
						else if (!detented)
						{
							lastValid = input.save;
							validKey = true;
						}
						input.skipAllWhiteGetDetent(detented);
						bool empty;
						if (input.matchText("="))
						{
							if (!detented)
								lastValid = input.save;
							input.skipAllWhiteGetDetent(detented);
							value = parseExpression();
							if (value !is null)
							{
								if (!detented)
									lastValid = input.save;
							}
							else
								errored = true;
						}
						else if (input.matchText(","))
						{
							if (!detented)
								lastValid = input.save;
							input.skipAllWhiteGetDetent(detented);
						}
						else
						{
							if (!identifier.content.length)
								break;
							empty = true;
						}

						if (!errored)
						{
							if (!detented)
								lastValid = input.save;
							input.skipAllWhiteGetDetent(detented);
						}

						ret.attributes ~= TagNode.Attribute(identifier, value);
						if (!detented)
							lastNonDetented = ret.attributes;

						if (!wasDetented && detented && validKey)
						{
							lastNonDetented = ret.attributes;
							lastNonDetentedHadValue = false;
						}

						if (empty && !input.front.content.among!(")", ","))
						{
							errored = true;
							break;
						}

						if (input.front.content == ",")
							input.popFront();
					}
					ret.attributesRange[1] = input.front.range[0];
					if (errored)
					{
						lastValid.errors = input.errors;
						ret.attributes = lastNonDetented;
						input = lastValid;
					}
				}
				if (!input.expect(TokenType.raw, ")") && detented)
				{
					lastValid.errors = input.errors;
					ret.attributes = lastNonDetented;
					input = lastValid;
				}
				if (detented && !lastNonDetentedHadValue && ret.attributes.length)
					ret.attributes[$ - 1].expr = null;
			}
			if (input.front.content == "<")
			{
				ret._fitInside = input.front;
				input.popFront;
			}
			else if (input.front.content == ">")
			{
				auto tmp = input.front;
				ret._fitOutside = tmp;
				input.popFront;
				if (input.front.content == "<")
				{
					tmp = input.front;
					input.popFront;
					ret._fitInside = tmp;
				}
			}
			if (input.front.content == "&")
			{
				ret._translated = input.front;
				input.popFront;
			}

			if (input.front.content == ".")
			{
				ret._contents = parseTextBlock();
			}
			else if (allowNested && input.front.content == ":")
			{
				input.popFront;
				input.skipAll(TokenType.whitespace);
				ret._contents = parseNestedTags();
			}
			else
			{
				ret._contents = parseTagContents();
			}

			ret._token.range[1] = input.indexEOL;
			return ret;
		}
		else
		{
			input = save;
			return null;
		}
	}

	StringTagContents parseTextBlock()
	{
		Token front = input.front;
		if (front.content == ".")
		{
			input.popFront;
			string content;
			input.skipAll(TokenType.identifier, TokenType.raw, TokenType.whitespace);
			if (input.expect(TokenType.newline))
			{
				input.skipAll(TokenType.newline);
				if (input.expect(TokenType.indent))
				{
					content = parseText(true, true);
					input.expect(TokenType.detent);
				}
			}
			front.range[1] = input.indexEOL;
			return new StringTagContents(front, content);
		}
		return null;
	}

	TagContents parseTagContents()
	{
		if (auto assignment = parseAssignment())
			return assignment;
		if (input.peek(TokenType.newline))
			return null;
		input.match(TokenType.whitespace);
		return parseTextLine();
	}

	Assignment parseAssignment()
	{
		auto tok = input.front;
		if (input.matchText("!="))
		{
			string content = parseText(false, false);
			tok.content = "!=";
			tok.range[1] = input.front.range[0];
			return new RawAssignment(tok, content);
		}
		else if (input.matchText("="))
		{
			string content = parseText(false, false);
			tok.range[1] = input.front.range[0];
			return new Assignment(tok, content);
		}
		return null;
	}

	/// Searches for a path of AST nodes lying within the specified offset.
	/// Params:
	///   offset = The cursor position to search AST nodes in.
	///   inclusiveStart = true if an AST [1 .. 3] should be matched for index 1.
	///   inclusiveEnd = true if an AST [1 .. 3] should be matched for index 3.
	/// Returns: A path of AST nodes starting at the broadest object (Document) down to the finest object.
	AST[] searchAST(size_t offset, bool inclusiveStart = true, bool inclusiveEnd = true)
	out(r; r.length > 0)
	{
		AST[] ret = [root];

		root.traverse((AST node, AST parent) {
			if (ret[$ - 1] != parent)
				return VisitResult.return_;
			auto range = node.token.range;
			if (!offset.withinRange(range, inclusiveStart, inclusiveEnd))
				return VisitResult.continue_;
			ret ~= node;
			return VisitResult.recurse;
		});

		return ret;
	}
}

bool withinRange(size_t offset, size_t[2] range, bool inclusiveStart = true, bool inclusiveEnd = true)
{
	if (inclusiveStart && inclusiveEnd)
		return offset >= range[0] && offset <= range[1];
	else if (inclusiveStart)
		return offset >= range[0] && offset < range[1];
	else if (inclusiveEnd)
		return offset > range[0] && offset <= range[1];
	else
		return offset > range[0] && offset < range[1];
}

void skipAllWhiteGetDetent(ref DietInput input, ref bool detented)
{
	auto c = input.skipAllCount(TokenType.whitespace, TokenType.detent,
			TokenType.indent, TokenType.newline);
	if (c[1])
		detented = true;
}

private void assertToken(Token token, TokenType type, string content)
{
	assert(token.type == type);
	assert(token.content == content);
}

private void assertToken(Token token, TokenType type, string content, size_t[2] range)
{
	assert(token.type == type);
	assert(token.content == content);
	assert(token.range == range);
}

unittest
{
	DietInput input;
	input.file = "stdin";
	input.code = q{doctype html
html

};

	auto parser = new ASTParser;
	parser.input = input.save;
	parser.parseDocument();

	assert(parser.input.errors.length == 0);

	assert(parser.root);
	assert(parser.root.token.range == [0, 17]);
	assert(parser.root.children.length == 2);

	auto doctype = cast(TagNode) parser.root.children[0];
	auto html = cast(TagNode) parser.root.children[1];

	assert(doctype);
	assert(html);

	doctype.tag.assertToken(TokenType.identifier, "doctype");
	html.tag.assertToken(TokenType.identifier, "html");
}

unittest
{
	DietInput input;
	input.file = "stdin";
	input.code = `foo
	.bar1 text1
	.bar2 text2
`;

	auto parser = new ASTParser;
	parser.input = input.save;
	parser.parseDocument();

	assert(parser.input.errors.length == 0);

	assert(parser.root);
	assert(parser.root.children.length == 1);
	auto root = cast(TagNode) parser.root.children[0];
	assert(root);
	root.tag.assertToken(TokenType.identifier, "foo");
	assert(root.children.length == 2);

	auto bar1 = cast(TagNode) root.children[0];
	auto bar2 = cast(TagNode) root.children[1];
	assert(bar1);
	assert(bar2);

	bar1.tag.assertToken(TokenType.identifier, "div", [5, 5]);
	bar2.tag.assertToken(TokenType.identifier, "div", [18, 18]);

	assert(cast(TextLine) bar1.contents,
			"Expected string contents but got " ~ bar1.contents.to!string);
	assert(cast(TextLine) bar2.contents,
			"Expected string contents but got " ~ bar2.contents.to!string);

	assert((cast(TextLine) bar1.contents)._parts.length == 1);
	assert((cast(TextLine) bar2.contents)._parts.length == 1);
	assert((cast(TextLine) bar1.contents)._parts[0].raw == "text1");
	assert((cast(TextLine) bar2.contents)._parts[0].raw == "text2");
}

unittest
{
	DietInput input;
	input.file = "stdin";
	input.code = `foo
	- int item = 3;
	p #{item.foo} bar
`;

	auto parser = new ASTParser;
	parser.input = input.save;
	parser.parseDocument();

	assert(parser.input.errors.length == 0);

	assert(parser.root);
	assert(parser.root.children.length == 1);
	auto root = cast(TagNode) parser.root.children[0];
	assert(root);
	root.tag.assertToken(TokenType.identifier, "foo");
	assert(root.children.length == 2);

	auto code = cast(DStatement) root.children[0];
	auto paragraph = cast(TagNode) root.children[1];
	assert(code);
	assert(paragraph);

	assert(code.content == " int item = 3;");
	paragraph.tag.assertToken(TokenType.identifier, "p");

	auto content = cast(TextLine) paragraph.contents;
	assert(content);
	assert(content._parts.length == 2);
	assert(content._parts[0].inlineExpr);
	assert(content._parts[0].inlineExpr.token.range[0] == 26);
	assert(content._parts[0].inlineExpr.token.range[1] == 34);
	assert(content._parts[0].inlineExpr.token.content == "item.foo");
	assert(content._parts[0].inlineExpr.content == "item.foo");
	assert(content._parts[1].raw == " bar");
}

unittest
{
	DietInput input;
	input.file = "stdin";
	input.code = `//-foo`;

	auto parser = new ASTParser;
	parser.input = input.save;
	parser.parseDocument();

	assert(parser.input.errors.length == 0);

	assert(parser.root);
	assert(parser.root.children.length == 1);
	auto root = cast(HiddenComment) parser.root.children[0];
	assert(root);
	assertToken(root.token, TokenType.raw, "//-");
	assert(root.content == "foo");
}

unittest
{
	DietInput input;
	input.file = "stdin";
	input.code = `-foo`;

	auto parser = new ASTParser;
	parser.input = input.save;
	parser.parseDocument();

	assert(parser.input.errors.length == 0);

	assert(parser.root);
	assert(parser.root.children.length == 1);
	auto root = cast(DStatement) parser.root.children[0];
	assert(root);
	assertToken(root.token, TokenType.raw, "-");
	assert(root.content == "foo");
}
