{ NotepadEx
  Copyright Air16 2022
  Licensed under LGPL

  see readme.md
  }
program NotepadEx;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm,
  lazcontrols,
  printer4lazarus;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
	Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.

