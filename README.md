# DietComplete

Join the chat: [![Join on Discord](https://discordapp.com/api/guilds/242094594181955585/widget.png?style=shield)](https://discord.gg/Bstj9bx)

An error correcting diet parser based on the diet-ng spec. See [SPEC.md](SPEC.md) for the added grammar by this repository.

## Example

```d
import std.algorithm;
import std.conv;
import std.stdio;

import dietc.complete;

void main()
{
	// Create completion engine from filename
	// You can also pass a parsed document with a callback for other files in the future (from extend tags)
	auto complete = new DietComplete("views/filename.dt");

	size_t offset = 12;
	// Use completeAt to get completion results for a specific byte offset in the code
	auto completion = complete.completeAt(offset);
	if (completion is Completion.completeD)
	{
		size_t dOffset;
		string dCode;
		complete.extractD(offset, /*out*/ dCode, /*out*/ dOffset);
		// special instruction that D code should be completed here
		writeln("Complete D code ", dCode, " at ", dOffset);
	}
	else
		completion.map!(a => text(a.type, ": ", a.text)).each!writeln;
}
```
