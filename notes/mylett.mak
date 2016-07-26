@Comment[ Copyright (C) 1979, 1980  UNILOGIC, Ltd. ]

@Marker(Make,MyLetter,Press,X9700)
@FontFamily(Helvetica 10)
@Define(Address,Nofill,LeftMargin 0,Break,Use R,Spacing 1,Spaces Kept,
	Sink 2.2in, above 0, below 0)
@Define(Body,Fill,Justification,Use R,LeftMargin 0,EofOK,
	Spacing 1,Spread 1,Spaces Compact,BlankLines Break,
	Sink 3.2in, Above 1 line,Below 0.5in,Break)
@Define(Ends,Nofill,LeftMargin 3.3in,Spread 0,Break,Use R,
	RightMargin 0,BlankLines Kept,Spacing 1)
@Define(ReturnAddress, use Ends, group, spaces kept)
@Define(Signature, use Ends, above 0)
@Define(Notations,Nofill,LeftMargin 0,Spread 0,Break,BlankLines Kept,
	RightMargin 0,Spaces Kept,Sink 8.0in)
@Define(PS=Body,Sink 0,Above 0,Below 0)
@Define(Greeting=Flushleft)
@Equate(PostScript=PS,PostScripts=PS,Closings=Notations,Initials=Notations)
@LibraryFile(Mathematics10)
@begin(Text,Justification,Font BodyFont,FaceCode R,LeftMargin 1.0in,
	LineWidth 6.3in,BottomMargin 1in,TopMargin 1.0in,
	Spacing 1,Indent 0)
@begin(Ends,Eofok)
@PageHeading(left "@value(Date)",right "Page @value(Page)")

@marker(Make,MyLetterhead,Press)
@string(Phone="(412) 268-7651",Department="Computer Science Department")
@String(Psychology="Department of Psychology",
	Math="Department of Mathematics",
	EPP="Engineering & Public Policy",
	Robotics="The Robotics Institute",
	EE="Department of Electrical Engineering")
@String(CSD="@q(@@)",PSI="@ @ @ @ @ @ @J(Y)@ ",
	None="@q[@ @ @ @ @ @ @ @ @ @ ]")
@String(Logo=CSD)
@DefineFont(LetterHeadFont,Q=<ascii "CMUlogo18">,R=<ascii "Helvetica10B">,
	     H=<ascii "Helvetica14B">,J=<ascii "Hippo18MRR">)
@FontFamily(Helvetica 10)
@Define(Q,FaceCode Q)
@Define(H,FaceCode H)
@Define(J,FaceCode J)
@Style(TopMargin 0.3in,WidowAction Force)
@Define(Address,Nofill,LeftMargin 0,Break,Use R,Spacing 1,Spaces Kept,
	Sink 2.2in, above 0, below 0)
@Define(Body,Fill,Justification,Use R,LeftMargin 0,EofOK,
	Spacing 1,Spread 1,Spaces Compact,BlankLines Break,
	TopMargin 1in,
	Sink 3.2in, Above 1 line,Below 0.5in,Break)
@Define(Ends,Nofill,LeftMargin 3.3in,Spread 0,Break,Use R,
	Above 1.6in,
	RightMargin 0,BlankLines Kept,Spacing 1)
@Equate(ReturnAddress = Comment)
@Define(Signature, use Ends, above 0)
@Define(Notations,Nofill,LeftMargin 0,Spread 0,Break,BlankLines Kept,
	RightMargin 0,Spaces Kept,Sink 8.0in)
@Define(LogoFormat=Format,Font LetterHeadFont,FaceCode R,Break,Above 0,
	Below 0,NoFill,Spacing 0.2in,LeftMargin -0.23in,RightMargin -0.23in,
	AfterEntry "@TabClear()")
@Define(PS=Body,Sink 0,Above 0,Below 0)
@Define(Greeting=Flushleft)
@Equate(PostScript=PS,PostScripts=PS,Closings=Notations,Initials=Notations)
@LibraryFile(Mathematics10)
@Begin(Text,Justification,Font BodyFont,FaceCode R,LeftMargin 1.0in,
	Indent 0,LineWidth 6.3in,BottomMargin 1in,TopMargin 1.3in,
	Spacing 1)
@TextForm(NewLetter={@NewPage()@Set(Page=1)
@begin(LogoFormat,Fixed 0.625in,ScriptPush on)
@value(Logo)@|@!@h[@value(Department)]@>@h[Carnegie Mellon University]@\
@end(LogoFormat)
@begin(LogoFormat,Fixed 1.2in)
@/@value(phone)@>Pittsburgh, Pennsylvania  15213@\
@end(LogoFormat)
})
@NewLetter()
@Begin(Ends,Eofok)
@PageHeading(left "@value(Date)",right "Page @value(Page)")
@TextForm(Make={@end(ends)@NewLetter()@Begin(Ends,Eofok)})
@Comment{

Document type definition file to define a personal letter
(i.e. not letterhead) on the Diablo HyType II.
}
