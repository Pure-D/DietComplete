module dietc.complete;

import dietc.lexer;
import dietc.parser;

import std.algorithm;
import std.array;
import std.string;
import std.uni;
import std.meta;

/// Delegate to provide other files that are being extended
alias FileProvider = DietInput delegate(string name);

enum CompletionType
{
	none,
	tag,
	attribute,
	value,
	reference,
	cssName,
	cssValue,
	d,
	meta
}

struct Completion
{
	CompletionType type;
	string text;
	string definition;
	string documentation;

	string referenceFile;
	size_t[2] referenceRange;
	bool preselected;

	auto preselect()
	{
		preselected = true;
		return this;
	}

	static immutable const(Completion)[] completeD = [Completion(CompletionType.d, "", "<d source>")];
}

struct TagInfo
{
	struct Attribute
	{
		string name;
		CompletionSource completion;
	}

	string tag;
	Attribute[] attributes;
}

private const(TagInfo)[] parseTagInfos(string info)
{
	const(TagInfo)[] ret;
	const(TagInfo.Attribute)[] attributeBase;
	CompletionSource[string] enumCompletions;

	auto parseAttribute(string attr)
	{
		auto colon = attr.indexOf(":");
		if (colon == -1)
			throw new Exception("Malformed attribute: " ~ attr);
		TagInfo.Attribute ret;
		ret.name = attr[0 .. colon];
		auto value = attr[colon + 1 .. $];
		auto dot = value.indexOf('.');

		if (auto exist = value in enumCompletions)
			ret.completion = *exist;
		else if (dot != -1)
			ret.completion = new AttributeValueByTagNameComplete(value[0 .. dot], value[dot + 1 .. $]);
		else
			throw new Exception("Unknown attribute value " ~ value);

		return cast(const) ret;
	}

	foreach (line; info.lineSplitter)
	{
		line = line.strip;
		if (line.startsWith("//") || !line.length)
			continue;
		if (line.startsWith("e "))
		{
			line = line[1 .. $].stripLeft;
			auto larr = line.indexOf("<");
			if (larr == -1)
				throw new Exception("Malformed enum line: " ~ line);
			auto name = line[0 .. larr].stripRight;
			auto values = line[larr + 1 .. $].stripLeft.splitter;
			enumCompletions[name] = new EnumComplete(values.map!(a => Completion(CompletionType.value, a)).array);
		}
		else if (line.startsWith("t "))
		{
			line = line[1 .. $].stripLeft;
			auto larr = line.indexOf("<");
			if (larr == -1)
				throw new Exception("Malformed enum line: " ~ line);
			auto name = line[0 .. larr].stripRight;
			auto attrs = attributeBase;
			foreach (attr; line[larr + 1 .. $].stripLeft.splitter)
				attrs ~= parseAttribute(attr);
			ret ~= const TagInfo(name, attrs);
		}
		else if (line.startsWith("tbase "))
		{
			line = line["tbase".length .. $].stripLeft;
			attributeBase = null;
			foreach (attr; line.splitter)
				attributeBase ~= parseAttribute(attr);
		}
		else throw new Exception("Invalid line " ~ line);
	}
	return ret;
}

__gshared const(TagInfo)[] tagInfos;
shared static this()
{
	tagInfos = import("html.txt").parseTagInfos;
}

static immutable Completion[] tagCompletions = import("tags.txt").strip
	.splitLines.map!(a => Completion(CompletionType.tag, a)).array;

//dfmt off
// https://github.com/rejectedsoftware/diet-ng/blob/f65a31def40f40cba2bf03a8f2093821e28a26d3/source/diet/html.d#L428
static immutable Completion[] doctypeCompletions = [
	Completion(CompletionType.value, "html", `<!DOCTYPE html>`).preselect,
	Completion(CompletionType.value, "xml", `<?xml version="1.0" encoding="utf-8" ?>`),
	Completion(CompletionType.value, "transitional", `<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">`),
	Completion(CompletionType.value, "strict", `<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">`),
	Completion(CompletionType.value, "frameset", `<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Frameset//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd">`),
	Completion(CompletionType.value, "1.1", `<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">`),
	Completion(CompletionType.value, "basic", `<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML Basic 1.1//EN" "http://www.w3.org/TR/xhtml-basic/xhtml-basic11.dtd">`),
	Completion(CompletionType.value, "mobile", `<!DOCTYPE html PUBLIC "-//WAPFORUM//DTD XHTML Mobile 1.2//EN" "http://www.openmobilealliance.org/tech/DTD/xhtml-mobile12.dtd">`),
];
//dfmt on

interface CompletionSource
{
	const(Completion)[] complete(string identifier, AST[] context, DietComplete engine, size_t offset);
}

class EnumComplete : CompletionSource
{
	const Completion[] available;

	this(const Completion[] available)
	{
		this.available = available;
	}

	const(Completion)[] complete(string identifier, AST[], DietComplete, size_t) const
	{
		return available.filter!(a => a.text.asLowerCase.startsWith(identifier.asLowerCase)).array;
	}
}

/// Finds attribute values from other tags based on a tag name such as "find id values from table elements".
class AttributeValueByTagNameComplete : CompletionSource
{
	string attribute, tag;

	this(string attribute, string tag)
	{
		this.attribute = attribute;
		this.tag = tag;
	}

	const(Completion)[] complete(string identifier, AST[] context, DietComplete, size_t) const
	{
		return null;
	}
}

class AttributeNameComplete : CompletionSource
{
	const TagInfo[] tagInfos;

	this(in TagInfo[] tagInfos)
	{
		this.tagInfos = tagInfos;
	}

	const(Completion)[] complete(string identifier, AST[] context, DietComplete, size_t) const
	{
		TagNode tag;
		foreach_reverse (node; context)
			if (cast(TagNode) node)
			{
				tag = cast(TagNode) node;
				break;
			}

		if (tag)
		{
			foreach (info; tagInfos)
			{
				if (info.tag == tag.name)
				{
					const(Completion)[] completion;
					foreach (attr; info.attributes)
						if (attr.name.startsWith(identifier))
							completion ~= Completion(CompletionType.attribute, attr.name);
					return completion;
				}
			}
		}
		return null;
	}
}

class DietComplete
{
	FileProvider provider;
	ASTParser parser;
	EnumComplete tags;
	EnumComplete doctypes;
	AttributeNameComplete attributeNames;

	this(string file)
	{
		import std.path : dirName;
		this(DietInput.fromFile(file), defaultFileProvider(dirName(file)));
	}

	this(DietInput root, FileProvider provider)
	{
		tags = new EnumComplete(tagCompletions);
		doctypes = new EnumComplete(doctypeCompletions);
		attributeNames = new AttributeNameComplete(tagInfos);

		this.provider = provider;
		root.reset();
		parser = ASTParser(root);
		parser.parseDocument();
	}

	void reparse(string content)
	{
		parser.input.code = content;
		parser.input.reset();
		parser.parseDocument();
	}

	static FileProvider defaultFileProvider(string dir)
	{
		return (name) {
			import std.file : exists;
			import std.path : chainPath, withExtension;

			if (exists(chainPath(dir, name.withExtension(".dt"))))
				return DietInput.fromFile(chainPath(dir, name.withExtension(".dt")));
			else
				return DietInput.init;
		};
	}

	const(Completion)[] completeAt(size_t offset)
	{
		auto tree = parser.searchAST(offset);

		auto contentLess = tree;
		if (cast(Document) contentLess[0])
			while (cast(TextLine) contentLess[$ - 1] || cast(TextLine.PartAST) contentLess[$ - 1])
				contentLess.length--;

		if (cast(DStatement) tree[$ - 1] || cast(Assignment) tree[$ - 1] || cast(RawAssignment) tree[$ - 1])
		{
			auto stmt = cast(IStringContainer) tree[$ - 1];
			if (offset.withinRange([stmt.token.range[0] + 1, stmt.token.range[0] + 1 + stmt.content.length]))
				return Completion.completeD;
			else
				return null; // before "-" character or in newline at end
		}

		if (auto expr = cast(Expression) tree[$ - 1])
		{
			auto code = expr.content;
			if (!code.all!isNumber && !code.isPlainString)
				return Completion.completeD;
		}

		if (tree.length >= 3)
		{
			if (auto part = cast(TextLine.PartAST) tree[$ - 1])
				if (auto line = cast(TextLine) tree[$ - 2])
					if (auto tag = cast(TagNode) tree[$ - 3])
						if (line._parts.length == 1 && part.part.raw.length)
						{
							string text = part.part.raw[0 .. offset - part.token.range[0]];
							if (tag.name == "doctype")
								return doctypes.complete(text, tree, this, offset);
						}
		}

		if (auto tag = cast(TagNode) contentLess[$ - 1])
		{
			if (offset.withinRange(tag.tag.range))
				return tags.complete(parser.input.read([tag.tag.range[0], offset]), contentLess, this, offset);
			else if (offset.withinRange(tag.attributesRange)) // somewhere random in attributes but not in name or value
				return attributeNames.complete("", contentLess, this, offset);
		}

		if (contentLess.length >= 2)
		{
			if (auto tag = cast(TagNode) contentLess[$ - 2])
				if (auto attr = cast(TagNode.AttributeAST) contentLess[$ - 1])
					if (offset.withinRange(attr.token.range))
						return attributeNames.complete(parser.input.read([attr.token.range[0], offset]), contentLess, this, offset);

			if (auto text = cast(TextLine) contentLess[$ - 1]) // must be an empty textline because otherwise a PartAST would be here
				if (auto tag = cast(TagNode) contentLess[$ - 2])
				{
					if (tag.name == "doctype")
						return doctypes.complete("", contentLess, this, offset);
				}
		}

		if (tree.length == 1)
		{
			auto ret = tags.complete("", tree, this, offset);
			if (cast(Document)tree[0] && (cast(Document)tree[0])._children.length == 0)
				ret ~= Completion(CompletionType.meta, "doctype").preselect;
			return ret;
		}

		return null;
	}
}

void extractD(DietComplete complete, size_t offset, out string code, out size_t codeOffset)
{
	return complete.parser.root.extractD(offset, code, codeOffset);
}

void extractD(AST root, size_t offset, out string code, out size_t codeOffset)
{
	codeOffset = size_t.max;
	class CodeVisitorImpl : ASTVisitor
	{
		override void visit(DStatement stmt) in (stmt !is null)
		{
			if (offset.withinRange(stmt.token.range))
				codeOffset = code.length + (offset - (stmt.token.range[0] + stmt.token.content.length));
			code ~= stmt.content;
			if (stmt.children.length)
			{
				code ~= "{/*children*/";
				stmt.accept(this);
				code ~= "}";
			}
		}

		static foreach (T; AliasSeq!(Expression, Assignment, RawAssignment))
			override void visit(T expr) in (expr !is null)
			{
				code ~= "__diet_value(";
				if (offset.withinRange(expr.token.range))
					codeOffset = code.length + (offset - expr.token.range[0] - expr.token.content.length);
				code ~= expr.content;
				code ~= ");";
			}

		override void visit(Comment comment) in (comment !is null)
		{
		}

		override void visit(HiddenComment comment) in (comment !is null)
		{
		}

		override void visit(Document doc) in (doc !is null)
		{
			code ~= "void __diet_document() {";
			doc.accept(this);
			code ~= "}";
		}

		static foreach (T; AliasSeq!(TagNode, TagNode.AttributeAST))
			override void visit(T ast) in (ast !is null)
			{
				code ~= "{";
				ast.accept(this);
				code ~= "}";
			}

		alias visit = ASTVisitor.visit;
	}

	new CodeVisitorImpl().visit(root);
}

bool isPlainString(string code)
{
	if (!code.length)
		return false;
	auto quote = code[0];
	if (quote != '\'' && quote != '"' && quote != '`')
		return false;
	if (code[$ - 1] != quote)
		return false;
	// TODO: better string check
	return true;
}

unittest
{
	import std.conv;

	const(Completion)[] testComplete(string text, size_t at,
			string file = __FILE__ ~ ":" ~ __LINE__.to!string)
	{
		DietInput input;
		input.file = file;
		input.code = text;
		return new DietComplete(input, cast(FileProvider)(name) {
			assert(false, "Can't import " ~ name ~ " in test");
		}).completeAt(at);
	}

	assert(testComplete("a", 0).canFind!(a => a.text == "textarea"));
	assert(testComplete("t", 1).canFind!(a => a.text == "textarea"));
	assert(testComplete("", 0).canFind!(a => a.text == "textarea"));
	assert(testComplete("div\n\t", 5).canFind!(a => a.text == "textarea"));
}
