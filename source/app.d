import std.algorithm;
import std.conv;
import std.stdio;
import std.string;

import dietc.complete;
import dietc.lexer;
import dietc.parser;

void main(string[] args)
{
	bool verbose;
	if (args.length > 1 && args.any!(a => a == "-v" || a == "--v" || a == "--verbose"))
		verbose = true;

	DietInput input;
	input.file = "stdin";
	input.code = q{doctype html
html(lang="de")
	head
		title Hello World
	body
		- foreach (index; 0 .. 10)
			ul= i};

	auto parser = new ASTParser;
	parser.input = input.save;
	parser.parseDocument();
	writeln("Input: ", input);

	parser.input.errors.map!"a.message".each!writeln;

	writeln("AST: ", parser.root);

	auto complete = new DietComplete(parser.input, (name) => DietInput.init);

	void testComplete(size_t n)
	{
		if (verbose)
			writeln("At ", n, ":\n", parser.searchAST(n).map!(a => "\t-(" ~ (cast(Object)a).classinfo.name ~ ")" ~ a.to!string.replace("\n", "\n\t\t")).join("\n"));
		else
			writeln("At ", n, ":\n", parser.searchAST(n).map!(a => "\t- " ~ (cast(Object)a).classinfo.name).join("\n"));

		auto res = complete.completeAt(n);
		if (res is Completion.completeD)
			writeln("<D Complete>");
		else
			writeln("Completion:\n", res.map!(a => text(a.preselected ? "*" : "", "[", a.type, "] ", a.text, " : ", a.definition)).join("\n"));
		writeln();
	}

	testComplete(24);

	string code;
	size_t offset;
	complete.extractD(98, code, offset);
	writeln("D:\n", code);
	writeln(offset == size_t.max ? "not found" : offset.to!string);
}
