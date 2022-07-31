{
	LogUtils
    LGPL Air16 2022

    Provides shortcuts to log data using LazLogger
}
unit LogUtils;

{$mode ObjFPC}{$H+}

interface

uses
	Classes, SysUtils, LazLogger;

function s(i:integer):string;
function s(i:int64):string;
function s(f:double):string;
function s(b:boolean):string;
procedure log(s:string);
procedure log(s:string;s2:string);
procedure log(i:integer);
procedure log(i1:integer;i2:integer);
procedure log(s1:string;i:integer);
procedure log(s1:string;i:integer;s2:string;i2:integer);
procedure log(s1:string;s2:string;s3:string;s4:string = '');
procedure log(s1:string;b:boolean);

implementation

function s(i:integer):string;begin result:=intToStr(i);end;
function s(i:int64):string;begin result:=intToStr(i);end;
function s(f:double):string;begin result:=floatToStr(f);end;
function s(b:boolean):string;begin result:=boolToStr(b,true);end;
procedure log(s:string);begin debugln(s);end;
procedure log(s:string;s2:string);begin debugln(s + '=', s2);end;
procedure log(i:integer);begin log(s(i));end;
procedure log(i1:integer;i2:integer);begin log(s(i1), s(i2));end;
procedure log(s1:string;i:integer);begin debugln(s1 + '=', s(i));end;
procedure log(s1:string;i:integer;s2:string;i2:integer);
begin debugln(s1 + '=', s(i), s2 + '=', s(i2)); end;
procedure log(s1:string;s2:string;s3:string;s4:string);
begin debugln(s1, s2, s3, s4);end;
procedure log(s1: string; b: boolean);begin log(s1, s(b));end;

end.

