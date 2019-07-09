module dietc.lexer;

import std.algorithm;
import std.ascii;
import std.conv;
import std.string;
import std.utf;

enum TokenType
{
	raw,
	indent,
	detent,
	identifier,
	code,
	newline,
	whitespace,
	eof
}

struct Token
{
	TokenType type;
	string content;
	size_t[2] range;
}

struct ErrorContext
{
	struct Error
	{
		string file;
		size_t at;
		string message;
	}

	Error[] errors;
	alias errors this;

	string formatMessage(ref DietInput input, size_t at, string s)
	{
		auto pos = input.code.bytesToPosition(at);
		return input.file ~ "(" ~ to!string(pos[0] + 1) ~ ":" ~ to!string(pos[1] + 1) ~ "): " ~ s;
	}

	void error(ref DietInput input, size_t at, string s)
	{
		errors ~= Error(input.file, at, formatMessage(input, at, s));
	}

	void expect(ref DietInput input, size_t at, string expectation,
			string srcfile = __FILE__, size_t srcline = __LINE__)
	{
		foreach (ref error; errors)
		{
			if (error.file == input.file && error.at == at && error.message.startsWith("Expected "))
			{
				error.message ~= ", " ~ expectation;
				return;
			}
		}
		string prefix;
		debug prefix = "(src=" ~ srcfile ~ ":" ~ srcline.to!string ~ ") ";
		errors ~= Error(input.file, at, prefix ~ formatMessage(input, at, "Expected " ~ expectation));
	}
}

/// zero-based [line, column]
alias Position = size_t[2];

Position bytesToPosition(string code, size_t bytes)
{
	size_t line, column;
	bool wasCR;
	size_t i;
	while (code.length)
	{
		if (i >= bytes)
			break;
		size_t len;
		immutable c = code.decodeFront(len);
		i += len;
		if (c == '\r')
		{
			wasCR = true;
		}
		else if (c == '\n')
		{
			line++;
			column = 0;
			wasCR = false;
		}
		else
		{
			if (wasCR)
			{
				line++;
				column = 0;
			}
			wasCR = false;
		}
	}
	if (wasCR)
	{
		line++;
		column = 0;
	}
	return [line, column];
}

enum IndentStyle
{
	unknown,
	spaces,
	tabs
}

struct DietInput
{
	string file;
	string code;
	size_t index;
	int tabSize = 4;
	size_t[] indentation;
	IndentStyle indentationStyle;
	Token last;
	Token[] backlog;
	bool lastWasNewline = true;

	size_t indexEOL() @property const
	{
		string pre = read([0, index]);
		if (pre.endsWith("\r\n"))
			return index - 2;
		else if (pre.endsWith("\r", "\n"))
			return index - 1;
		else
			return index;
	}

	static DietInput fromFile(R)(R file)
	{
		import std.file : readText;

		DietInput ret;
		ret.code = readText(file);
		ret.file = file.to!string;
		return ret;
	}

	ErrorContext errors;

	size_t determineIndentation(string whitespace, out bool error)
	{
		assert(whitespace.byDchar.all!isWhite);
		size_t indentation; // @suppress(dscanner.suspicious.label_var_same_name)
		foreach (c; whitespace.byDchar)
		{
			if (c == '\t')
			{
				if (indentationStyle == IndentStyle.unknown)
					indentationStyle = IndentStyle.tabs;
				else if (indentationStyle == IndentStyle.spaces)
					error = true;
				indentation = (indentation / tabSize + 1) * tabSize;
			}
			else
			{
				if (indentationStyle == IndentStyle.unknown)
					indentationStyle = IndentStyle.spaces;
				else if (indentationStyle == IndentStyle.tabs)
					error = true;
				indentation++;
			}
		}
		return indentation;
	}

	string read(size_t[2] range) const
	{
		if (range[1] < range[0])
			return null;
		if (range[0] < 0)
			range[0] = 0;
		if (range[1] > code.length)
			range[1] = code.length;
		return code[range[0] .. range[1]];
	}

	void reset()
	{
		index = 0;
		indentation.length = 0;
		backlog.length = 0;
		last = Token.init;
		indentationStyle = IndentStyle.unknown;
		lastWasNewline = true;
	}

	/// Checks if the current token matches type & optional match.
	bool peek(TokenType type, string match = null)
	{
		auto t = front();
		if (t.type != type)
			return false;
		if (match !is null)
			return t.content == match;
		return !empty || type == TokenType.eof;
	}

	/// Does a peek and advances a token.
	bool match(TokenType type, string match = null)
	{
		auto ret = peek(type, match);
		popFront();
		return ret;
	}

	/// Does a peek and advances a token and adds an error if it doesn't match.
	bool expect(TokenType type, string match = null, string srcfile = __FILE__,
			size_t srcline = __LINE__)
	{
		size_t at = index;
		Token tok = front;
		auto ret = this.match(type, match);
		if (!ret)
			errors.expect(this, at, (match.length
					? "'" ~ match ~ "'" : type.to!string) ~ ", but got " ~ tok.to!string, srcfile, srcline);
		return ret;
	}

	size_t skipAll(TokenType[] types...)
	{
		size_t n;
		while (types.canFind(front.type))
		{
			popFront();
			n++;
		}
		return n;
	}

	size_t[] skipAllCount(TokenType[] types...)
	{
		size_t[] n = new size_t[](types.length);
		while (true)
		{
			auto i = types.countUntil(front.type);
			if (i < 0)
				break;
			popFront();
			n[i]++;
		}
		return n;
	}

	auto save()
	{
		auto copy = this;
		copy.indentation = indentation.dup;
		copy.backlog = backlog.dup;
		copy.errors = errors.dup;
		return copy;
	}

	void popFront()
	{
		if (backlog.length)
		{
			lastWasNewline = backlog[$ - 1].type == TokenType.newline;
			backlog.length--;
		}
		else if (index >= code.length)
		{
			index++;
			if (index > code.length + 100)
				throw new Exception("Attempted to read past EOF too often");
		}
	}

	bool empty() @property
	{
		return index >= code.length && backlog.length == 0 && indentation.length == 0;
	}

	auto front() @property
	{
		if (index >= code.length && backlog.length == 0 && indentation.length > 0)
		{
			if (indentation.length)
			{
				backlog.length = indentation.length + 1;
				backlog[0] = Token(TokenType.newline, null, [index, index]);
				foreach (i, indent; indentation)
					backlog[i + 1] = Token(TokenType.detent, null, [index, index]);
				backlog.reverse();
			}
			else
			{
				backlog = null;
			}
			indentation.length = 0;
		}
		if (index >= code.length && backlog.length == 0)
			return Token(TokenType.eof, null, [index, index]);
		else
		{
			while (!backlog.length)
			{
				backlog = parse();
				backlog.reverse();
			}
			return last = backlog[$ - 1];
		}
	}

	private Token[] parse()
	{
		const size_t start = index;
		if (index >= code.length)
		{
			Token[] ret;
			foreach_reverse (indent; indentation)
			{
				ret ~= Token(TokenType.detent, null, [start, index]);
				indentation.length--;
			}
			ret ~= Token(TokenType.eof, null, [start, index]);
			return ret;
		}
		size_t dummy = index;
		auto c = decode(code, dummy);
		const size_t cLength = dummy - index;

		// skip start of file whitespace
		if (index == 0 && c.isWhite)
		{
			size_t prev;
			while (c.isWhite)
			{
				prev = index;
				c = decode(code, index);
			}
			index = prev;
			return [];
		}

		if (c == '\r')
		{
			index++;
			if (index < code.length && code[index] == '\n')
				index++;
			return [Token(TokenType.newline, code[start .. index], [start, index])];
		}
		else if (c == '\n')
		{
			index++;
			return [Token(TokenType.newline, code[start .. index], [start, index])];
		}
		else if (c.isWhiteButNotNewline)
		{
			const isIndent = last.type == TokenType.newline;
			string data = code[start .. $];
			bool uselessWhitespace;
			while (data.length)
			{
				size_t len;
				c = data.decodeFront(len);
				if (c == '\r' || c == '\n')
					uselessWhitespace = true;
				if (!c.isWhiteButNotNewline)
					break;
				index += len;
			}
			assert(start != index);
			if (uselessWhitespace && lastWasNewline)
				return [];
			if (isIndent)
			{
				bool error;
				auto level = determineIndentation(code[start .. index], error);
				if (error)
					errors.error(this, start, "Mixing spaces and tabs indentation");
				Token[] ret;
				if (!indentation.length || indentation[$ - 1] < level)
				{
					ret ~= Token(TokenType.indent, code[start .. index], [start, index]);
					indentation ~= level;
				}
				else
				{
					foreach_reverse (indent; indentation)
					{
						if (indent <= level)
							break;
						ret ~= Token(TokenType.detent, code[start .. index], [start, index]);
						indentation.length--;
					}
				}
				return ret;
			}
			else
				return [
					Token(TokenType.whitespace, code[start .. index], [start, index])
				];
		}
		else if (c.tagIdentifierValidator)
		{
			string data = code[start .. $];
			while (data.length)
			{
				size_t len;
				c = data.decodeFront(len);
				if (!c.tagIdentifierValidator)
					break;
				index += len;
			}
			return [Token(TokenType.identifier, code[start .. index], [start, index])];
		}
		else
		{
			index += cLength;
			return [Token(TokenType.raw, code[start .. index], [start, index])];
		}
	}
}

/// Advances the input and returns true in case the tokens start with this match.
/// Warning: on match this will start a next token if it contains part of the match, discarding it basically.
bool matchText(ref DietInput input, string match)
{
	auto past = input.save();
	size_t i = 0;
	while (i < match.length && !input.empty)
	{
		auto p = input.front;
		input.popFront();
		if (match.length - i >= p.content.length)
		{
			if (match[i .. i += p.content.length] != p.content)
			{
				input = past;
				return false;
			}
		}
		else
		{
			if (match[i .. $] != p.content[0 .. match.length - i])
			{
				input = past;
				return false;
			}
			i = match.length;
		}
	}
	return i >= match.length;
}

bool isWhiteButNotNewline(dchar c)
{
	return c.isWhite && c != '\n' && c != '\r';
}

alias tagIdentifierValidator = (c) => c == '-' || c == ':' || c == '_' || c.isAlphaNum;
/// [-:_0-9a-zA-Z]+
bool validateTagIdentifier(string s)
{
	return s.byDchar.all!tagIdentifierValidator;
}

/// [-_0-9a-zA-Z]+
bool validateIdentifier(string s)
{
	return s.byDchar.all!(c => c == '-' || c == '_' || c.isAlphaNum);
}

/// [-_a-zA-Z] [-_0-9a-zA-Z]*
bool validateIdentifierAlpha(string s)
{
	bool first = true;
	return s.byDchar.all!((c) {
		auto ret = c == '-' || c == '_' || c.isAlpha || (!first && c.isDigit);
		first = false;
		return ret;
	});
}

string indent(string s, string indentation = "\t")
{
	return s.lineSplitter!(KeepTerminator.yes)
		.map!(a => a.strip.length ? indentation ~ a : a)
		.join("");
}

unittest
{
	import std.array : array;

	DietInput input;
	input.file = "stdin";
	input.code = q{doctype html
html

};

	auto tokens = input.array;
	assert(input.errors.length == 0);

	with (TokenType)
		assert(tokens == [
				Token(identifier, "doctype", [0, 7]), Token(whitespace, " ", [7, 8]),
				Token(identifier, "html", [8, 12]), Token(newline, "\n", [12, 13]),
				Token(identifier, "html", [13, 17]), Token(newline, "\n", [17, 18]),
				Token(newline, "\n", [18, 19])
				]);
}
