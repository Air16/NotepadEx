unit AboutForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Button1: TButton;
    Label1: TLabel;
	Label2: TLabel;
	WebLink: TLabel;
	procedure FormCreate(Sender: TObject);
	procedure WebLinkClick(Sender: TObject);
  end;

implementation

uses
  FileInfo, lclintf;

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
var
  FileVerInfo: TFileVersionInfo;
  s:string;
begin
  FileVerInfo := TFileVersionInfo.create(nil);
  try
    FileVerInfo.readFileInfo();
    s := FileVerInfo.VersionStrings.Values['ProductName'];
	s += ' v' + FileVerInfo.VersionStrings.Values['FileVersion'];
    Label2.caption := s;
  finally
    FileVerInfo.free;
  end;
end;

procedure TAboutForm.WebLinkClick(Sender: TObject);
begin
    OpenDocument(weblink.Caption);
end;

end.

