{
	LineEndingUtils

    LGPL Air16 2022

    provides a function named GetLineEndingRec that parses a string and detects
    line ending chars : CRLF, CR, LF or UNKNOWN
}
unit LineEndingUtils;

{$mode ObjFPC}{$H+}

interface

type
  TLineEndingRec = record
    name:string;
    seq:string;
    isAccurate:boolean; // used when guessing in GetLineEndingRec
    LineBreakStyle:TTextLineBreakStyle;
  end;

const
  	CHAR_CR = #13;//^M;
	CHAR_LF = #10;//^J;

    LINE_ENDING_WINDOWS:TLineEndingRec = (
    	name:'Windows (CRLF)';
        seq:CHAR_CR + CHAR_LF;
        isAccurate:true;
        LineBreakStyle:tlbsCRLF
        );

    LINE_ENDING_UNIX:TLineEndingRec = (
    	name:'Unix (LF)';
        seq:CHAR_LF;
        isAccurate:true;
        LineBreakStyle:tlbsLF
        );

    LINE_ENDING_MACOS9:TLineEndingRec = (
    	name:'Macintosh (CR)';
        seq:CHAR_CR;
        isAccurate:true;
        LineBreakStyle:tlbsCR
        );

function GetLineEndingRec(s:string):TLineEndingRec;
// returns system line ending pattern
function DefaultLineEndingRec():TLineEndingRec;

implementation

uses
    SysUtils;

function DefaultLineEndingRec():TLineEndingRec;
begin
     result := LINE_ENDING_WINDOWS;
    if (LineEnding = #10) then
    	result := LINE_ENDING_UNIX
    else if (LineEnding = #13) then
        result := LINE_ENDING_MACOS9;
end;

function GetLineEndingRec(s:string):TLineEndingRec;
var _cr, _lf, i:integer;
begin
    // count _cr & _lf occurrences
  	 _cr:=0; _lf:=0;
	 for i:=1 to Length(s) do begin
	     if s[i] = CHAR_LF then inc(_lf)
         else if s[i] = CHAR_CR then inc(_cr);
	 end;

     // main case
     Result.name := '';
  	 if _cr + _lf <= 0 then begin
     	Result.name := 'n.a.';
        Result.seq:= LineEnding;
        Result.LineBreakStyle:=DefaultTextLineBreakStyle;
        Result.isAccurate:=false;
	 end
	 else if _cr = _lf then
     	result := LINE_ENDING_WINDOWS
     else if (_cr = 0) then
        result := LINE_ENDING_UNIX
	 else if (_lf = 0) then
        result := LINE_ENDING_MACOS9;

     if Length(result.name) > 0 then exit;

     // guess...
     if (_cr >= 2*_lf) then
        result := LINE_ENDING_MACOS9
	 else if (_lf >= 2*_cr) then
        result := LINE_ENDING_UNIX
	 else
       result := LINE_ENDING_WINDOWS;

     Result.isAccurate := false;
     Result.name := 'Prob. ' + Result.name;

end;

end.

