doc: parmenides.PS parmenides.doc

parmenides.press: parmenides.mss
	scribe parmenides.mss -Dover

parmenides.PS: parmenides.mss
	scribe parmenides.mss -device:postscript

parmenides.doc: parmenides.mss
	scribe parmenides.mss -FILE

