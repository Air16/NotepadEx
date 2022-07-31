{ see readme.md }
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Forms, Menus, Dialogs, StdCtrls, ComCtrls, ActnList, Controls, StdActns,
  ExtCtrls, PrintersDlgs, Classes,
  // Air16 lib
  MemoUtils, WinMemo, WinFileDialogs, EasyRegistry, BasicScrollbar;

type

  { TForm1 }

  TForm1 = class(TForm)
    AboutAction1: TAction;
	reg: TEasyRegistry;
	ExIsDarkTheme: TAction;
	ExReplaceWithCaretPos: TAction;
	ExOpenBackupFolder: TAction;
	ExAutoBackupEnabled: TAction;
	FileSaveAs1: TAction;
	FileOpen1: TAction;
	ButtonLabel: TLabel;
	Memo1: TWinMemo;
	MenuEx: TMenuItem;
	MenuItem10: TMenuItem;
	MenuItem11: TMenuItem;
	MenuItem24: TMenuItem;
	MenuItem25: TMenuItem;
	DarkPanel: TPanel;
	ButtonPanel: TPanel;
	ReplaceDialog1: TReplaceDialog;
	EditFindPrev1: TAction;
	EditFindNext1: TAction;
    FileNewWindow1: TAction;
    FileSave1: TAction;
    MenuItem1: TMenuItem;
	MenuEditReplace1: TMenuItem;
    MenuItem21: TMenuItem;
    MenuEditRedo1: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
	MenuItem7: TMenuItem;
    PageSetupDialog1: TPageSetupDialog;
    FilePrint1: TAction;
    FilePageSetup1: TAction;
    FileNew1: TAction;
    FileExit1: TFileExit;
    FormatFontEdit1: TFontEdit;
    EditInsertDateTime: TAction;
	AutoBackupTimer: TTimer;
	LoadBackupTimer: TTimer;
	MemoScrollbar: TWinMemoScrollbar;
	Separator6: TMenuItem;
	Separator8: TMenuItem;
    ViewZoomReset: TAction;
    ViewZoomOut: TAction;
    ViewZoomIn: TAction;
    FormatWordwrap: TAction;
    ApplicationProperties1: TApplicationProperties;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditDelete1: TEditDelete;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    MenuFormatWordwrap: TMenuItem;
    MenuFormatFont: TMenuItem;
    ViewStatusBarShowHide: TAction;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuEditUndo1: TMenuItem;
    MenuEditCut1: TMenuItem;
    MenuEditCopy1: TMenuItem;
    MenuEditPaste1: TMenuItem;
    MenuEditDelete1: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem3: TMenuItem;
    MenuEdit: TMenuItem;
    MenuFormat: TMenuItem;
    MenuZoom: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuView: TMenuItem;
    MenuHelp: TMenuItem;
    MenuViewStatusbar: TMenuItem;
    PrintDialog1: TPrintDialog;
    EditFind1: TSearchFind;
	EditReplace1: TSearchReplace;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    StatusBar1: TStatusBar;
	CmdLineTimer: TTimer;
    procedure AboutAction1Execute(Sender: TObject);
	procedure AutoBackupTimerTimer(Sender: TObject);
	procedure ButtonPanelClick(Sender: TObject);
	procedure ButtonPanelMouseEnter(Sender: TObject);
	procedure ButtonPanelMouseLeave(Sender: TObject);
	procedure CmdLineTimerTimer(Sender: TObject);
	procedure EditReplace1BeforeExecute(Sender: TObject);
	procedure ExAutoBackupEnabledExecute(Sender: TObject);
	procedure ExIsDarkThemeExecute(Sender: TObject);
	procedure ExOpenBackupFolderExecute(Sender: TObject);
	procedure ExReplaceWithCaretPosExecute(Sender: TObject);
    procedure FileNewWindow1Execute(Sender: TObject);
	procedure FileOpen1Execute(Sender: TObject);
	procedure FileSaveAs1Execute(Sender: TObject);
	procedure FormatFontEdit1BeforeExecute(Sender: TObject);
    procedure FormatWordwrapExecute(Sender: TObject);
	procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
	procedure LoadBackupTimerTimer(Sender: TObject);
	procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
	procedure Memo1KeyPress(Sender: TObject; var Key: char);
	procedure MemoScrollbarExit(Sender: TObject);
	procedure MenuEditRedo1Click(Sender: TObject);
	procedure MenuEditUndo1Click(Sender: TObject);
	procedure StatusBar1Resize(Sender: TObject);
	procedure TReplaceDialogClose(Sender: TObject);
    procedure ViewZoomInExecute(Sender: TObject);
    procedure ViewZoomOutExecute(Sender: TObject);
    procedure ViewZoomResetExecute(Sender: TObject);
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure FileNew1Execute(Sender: TObject);
    procedure FileSave1Execute(Sender:TObject);
    procedure FormatFontEdit1Accept(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure EditInsertDateTimeExecute(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure Memo1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Memo1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure ViewStatusBarShowHideExecute(Sender: TObject);
    procedure FilePageSetup1Execute(Sender: TObject);
    procedure FilePrint1Execute(Sender: TObject);
    //
    function PromptForFileSave(Sender: TObject):boolean;
    procedure FindNext(Sender:TObject);
    procedure FindPrev(Sender:TObject);
    procedure EditFind1BeforeExecute(Sender: TObject);
	procedure TFindDialogFind(Sender: TObject);
	procedure TReplaceDialogFind(Sender: TObject);
	procedure TReplaceDialogReplace(Sender: TObject);
  private
    MemoUndo:TMemoUndo;
    procedure SetTextMargin();
    procedure ResetUndo();
    procedure newBlankContent();
    procedure UpdateStatusBarStatus();
    procedure UpdateStatusBarCaret();
    procedure UpdateTitle();
    function IsFindLoopDenied():boolean;
    procedure OpenFile(Filename:string; encodingIndex:integer);
    function SaveFile(Filepath:string; silentErrors:boolean):string;
    procedure LoadCommandLineArguments();
    procedure ClearBackup();
    procedure FilenameUpdated();
    procedure UpdateAppColors();
    procedure OnMemoHistoryChanged(Sender:TObject);
    procedure SetTheme();
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses LazUnicode, AboutForm, LineEndingUtils, LConvEncoding,
  LogUtils, LazFileUtils, Process, Windows, Graphics, SysUtils,
  LCLIntf, FileUtil, Air16Utils;

var
  // curr content
  CurrFilename:string = '';
  CurrEncodingIndex:integer; // index in AVAILABLE_ENCODINGS and LCONV_ENCODINGS arrays
  CurrEncoding:string; // same as AVAILABLE_ENCODINGS except for ANSI
  CurrLineEndingRec:TLineEndingRec;
  CurrTitleFilename:string = ''; // set by FilenameUpdated()
  ContentHasChanged:boolean = false;
  // find/replace
  LastFindDialog:TFindDialog = nil; // nil until the first CTRL+F or CTRL+H
  SearchIsDone:boolean = true; // used for replaceAll

  // zooming
  MemoBaseFontSize:integer;
  CurrZoomLevel:Integer = 100;

  // auto backup
  BackupFolderPath:string;
  CurrBackupFilename:string;
  CurrFileIsABackupFile:boolean;
  RestoreMutexHandle:THandle = 0;

const
  DEFAULT_STATUSBAR_ISVISIBLE = true;
  DEFAULT_WORDWRAP = true;
  DEFAULT_AUTOBACKUP = true;
  DEFAULT_SHOW_CARETPOS = true;
  DEFAULT_DARK_THEME = false;

  // those two arrays MUST have same length
  AVAILABLE_ENCODINGS : array of string = (
  	'ANSI', 'UTF-16 LE', 'UTF-16 BE', 'UTF-8', 'UTF-8 with BOM');
  LCONV_ENCODINGS : array of string = (
  	'ansi', 'ucs2le', 'ucs2be', 'utf8', 'utf8bom');

  REG_KEY = 'Software\NotepadEx';
  REG_BOOL_STATUSBAR = 'statusBar';
  REG_BOOL_WORDWRAP = 'wordwrap';
  REG_BOOL_AUTOBACKUP = 'autoBackup';
  REG_BOOL_SHOW_CARETPOS = 'showCaretPosition';
  REG_FONT = 'font';
  REG_BOOL_DARK_THEME = 'darkTheme';

  BACKUP_MUTEX_NAME = 'restoring';

  // colors
  // default theme
  DEFAULT_BG_NORMAL = clDefault;
  DEFAULT_BG_BACKUPFILE = $00E1F2FB;
  DEFAULT_FONT = clDefault;
  // dark theme
  DARKTHEME_BG_NORMAL = clBlack;
  DARKTHEME_BG_BACKUPFILE = $004F2E00;
  DARKTHEME_FONT = clSilver;

{ TForm1 }

// dummy arg may be inserted to avoid the new window to open backup files
procedure OpenNewWindow(filename:string; withDummyArg:boolean = true);
var Process:TProcess; I:integer;
begin
   Process:=TProcess.create(nil);
   try
	Process.InheritHandles:=false;
	Process.Options:=[];
	Process.ShowWindow:=swoShow;
	Process.Executable:=Application.ExeName;
	// Copy default environment variables for GUI application to work
	for I := 1 to GetEnvironmentVariableCount do
		Process.Environment.Add(GetEnvironmentString(I));
	// add filename parameter
	if (length(filename) > 0) then
		Process.Parameters.Add('"' + filename + '"')
      else if withDummyArg then
          Process.Parameters.add(' dummy');
	Process.Execute;
   finally
     Process.Free;
   end;
end;

procedure TForm1.FileNewWindow1Execute(Sender: TObject);
begin
  OpenNewWindow('');
end;

const z_BACKUP = 0;

function IsBackupFile(filename:string):boolean;
begin
 result := extractFilePath(filename) = BackupFolderPath;
end;

function GetBackupFolderPath():string;
begin
 result := GetEnvironmentVariable('APPDATA') + DirectorySeparator
   + 'NotepadEx' + DirectorySeparator + 'Backup' + DirectorySeparator;
end;

function GetBackupFilename():string;
var randomStr:string;
begin
 	result := ExtractFileName(CurrFilename);
    if Length(result) <= 0 then begin
      randomize();
      randomStr := IntToStr(round(random() * 1000));
    	result := 'Untitled - ' + FormatDateTime('YYYY-MM-DD hh-mm-ss zzz', Now) + randomStr + '.backup';
	end;
end;

procedure SetCurrAndBackupFilenames(_openedFilename:string);
begin
	CurrFileIsABackupFile := false;
    if ExtractFilePath(_openedFilename) = BackupFolderPath then begin
       CurrFilename := '';
       CurrBackupFilename := ExtractFileName(_openedFilename);
       CurrFileIsABackupFile := true;
       log('file is a backup file');
       log('   filename=' + CurrFilename);
       log('   backup  =' + CurrBackupFilename);
	end
    else begin
    	CurrFilename := _openedFilename;
        CurrBackupFilename := GetBackupFilename();
	end;
    log('backup filename=' + CurrBackupFilename);
end;

procedure TForm1.ClearBackup();
begin
    AutoBackupTimer.enabled := false;
    try
    if fileExists(BackupFolderPath + CurrBackupFilename) then
        DeleteFile(BackupFolderPath + CurrBackupFilename);
	finally
	end;
end;

procedure TForm1.ExOpenBackupFolderExecute(Sender: TObject);
begin
    try
		openDocument(BackupFolderPath);
	except
		Application.MessageBox(
        pChar('Error opening backup folder "' + BackupFolderPath + '"'),
        'NotepadEx',
        MB_OK);
	end;
end;

function createRestoreMutex():boolean;
begin
	RestoreMutexHandle := CreateMutex(nil, true, BACKUP_MUTEX_NAME);
    result := GetLastError <> ERROR_ALREADY_EXISTS;
end;

procedure TForm1.LoadBackupTimerTimer(Sender: TObject);
var fileList:TStringList;
  i:integer;
  canOpen:boolean;
begin
	if not ExAutoBackupEnabled.checked then exit;
	LoadBackupTimer.enabled := false;

    // existing instance that already loaded backup files ?
	if not createRestoreMutex() then
	    exit;

    //debugln('Created restore mutex, restoring backup files...');

    fileList := TStringList.create();

    try
		// get backup file list - fileList is filled up with found files
		FindAllFiles(fileList, BackupFolderPath, '*.backup', false);

	    //debugln('found ' + IntToStr(fileList.count) + ' backup files');
	    canOpen := true;

	    if fileList.Count >= 10 then
	        canOpen := false
	    else if fileList.count >= 4 then
	        canOpen := (IDYES = Application.MessageBox(
	        	pChar('There are ' + IntToStr(fileList.Count) + ' backup files.'
	            + ' Press YES to open files in separate windows, '
                + 'NO to open the backup folder.'),
	            'NotepadEx',
	            MB_YESNO));

		if canOpen then begin
		    for i:=0 to fileList.Count-1 do
	    	    openNewWindow(fileList[i]);
		end
	    else
		    ExOpenBackupFolderExecute(Sender);

	finally
      fileList.free;
	end;
end;

procedure TForm1.AutoBackupTimerTimer(Sender: TObject);
begin
  	// first, very important : disable AutoBackupTimer
	AutoBackupTimer.Enabled:=false;

    // tag is used to count key pressed since AutoBackupTimer has started
    if AutoBackupTimer.Tag > 0 then begin
       //debugln('backup timer elapsed, but Tag=' + IntToStr(AutoBackupTimer.Tag));
       AutoBackupTimer.Tag:=0;
       // start again the timer
       AutoBackupTimer.Enabled:=true;
       exit;
	end;

    // we're here because no more change occurred on Memo during one Timer interval

    // and backup (or delete backup file if Memo is empty)
     try
	 if Length(CurrBackupFilename) > 0 then
         if length(memo1.Text) > 0 then
		    saveFile(BackupFolderPath + CurrBackupFilename, false)
         else
             deleteFile(BackupFolderPath + CurrBackupFilename);
        UpdateStatusBarStatus();
	 finally
	 end;
end;

// converts guessEncoding() result to user-friendly name + index (in AVAILABLE_ENCODINGS array)
procedure lConvToAvailableEncoding(lConv:string; var name:string; var index:integer);
var i, idx:integer;
begin
  idx := -1;
  for i:=0 to Length(LCONV_ENCODINGS)-1 do
      if (LCONV_ENCODINGS[i] = lconv) then begin
      	idx := i;
  		break;
        end;

  if (idx >= 0) then begin
    name := AVAILABLE_ENCODINGS[idx];
    index := idx;
    exit;
    end;

  // otherwise we have a CP so it is ansi
  name := 'ANSI (' + lConv + ')';
  index := 0;
end;

const z_FIND = 0;

procedure TForm1.TFindDialogFind(Sender: TObject);
begin
    if frDown in EditFind1.Dialog.Options then
       FindNext(Sender)
    else
        FindPrev(Sender);
end;

procedure TForm1.TReplaceDialogClose(Sender: TObject);
begin
    Form1.setfocus;
end;

procedure TForm1.TReplaceDialogFind(Sender: TObject);
begin
	if frDown in EditReplace1.Dialog.Options then
       findNext(Sender)
    else
        FindPrev(Sender);
end;

procedure TForm1.TReplaceDialogReplace(Sender: TObject);

	procedure replaceSelTextWith(newStr:string);
	var oldLeft, oldRight:string;
	oldSelStart:integer;
    begin
      oldSelStart := Memo1.SelStart;
      oldLeft := CodePointCopy(Memo1.Text, 1, oldSelStart);
      oldRight := CodePointCopy(Memo1.Text, oldSelStart + memo1.selLength + 1, CodePointLength(memo1.text));
      //Memo1.Text := oldLeft + oldRight;
      Memo1.Text := oldLeft + newStr + oldRight;
      Memo1.SelStart := oldSelStart;
      Memo1.SelLength := Length(newStr);
	end;

var safetyCounter:integer;
	strToReplace, strToReplaceWith:string;
begin
  	strToReplace := EditReplace1.Dialog.FindText;
	strToReplaceWith := EditReplace1.Dialog.ReplaceText;
    // need to continue ?
    if CodePointLength(strToReplace) <= 0 then exit;
    if strToReplace = strToReplaceWith then exit;
    // call findNext if there is no selected text
  	if memo1.selText <> strToReplace then
	    FindNext(Sender);
    // replaceAll
    if frReplaceAll in EditReplace1.Dialog.Options then begin
      	// save and freeze Undo buffer
	     MemoUndo.saveForUndo();
	     MemoUndo.Paused:=true;
         // avoid infinite loop
		safetyCounter := round(CodePointLength(Memo1.Text) / CodePointLength(strToReplace)) + 1;
		// go
       while true do begin
         replaceSelTextWith(strToReplaceWith);
         findNext(Sender);
         if SearchIsDone then break;
		 safetyCounter -= 1;
         if safetyCounter <= 0 then break;
	   end;
       // unfreeze and save
       MemoUndo.Paused:=false;
      // force saveForUndo as memo1.text.length may have not changed
      MemoUndo.saveForUndo(true);

      ContentHasChanged := true;
	  UpdateTitle();
	end
    // replace single
	else if frReplace in EditReplace1.Dialog.Options then begin
       replaceSelTextWith(strToReplaceWith);
       // force saveForUndo as memo1.text.length may have not changed
       MemoUndo.saveForUndo(true);
       ContentHasChanged := true;
       UpdateTitle();
	end;
end;

procedure TForm1.EditReplace1BeforeExecute(Sender: TObject);
begin
  // initialize FindDialog text with selected text if any
  EditReplace1.Dialog.FindText := Memo1.SelText;
  LastFindDialog := EditReplace1.Dialog;

end;

procedure TForm1.EditFind1BeforeExecute(Sender: TObject);
begin
  // initialize FindDialog text with selected text if any
  EditFind1.Dialog.FindText := Memo1.SelText;
  LastFindDialog := EditFind1.Dialog;
end;

// returns true if the Find dialog denies loop
function TForm1.IsFindLoopDenied():boolean;
begin
     if (LastFindDialog = nil) then begin
        Result := false;
        exit;
	 end;
	 Result := not (frEntireScope in LastFindDialog.Options);
end;

// loop by default, except if denied by FindDialog
procedure TForm1.FindNext(Sender:TObject);
var res:integer;
  CurrFindText:string;
begin
  //DebugLn('FindNext selStart=' + IntToStr(Memo1.SelStart) + ' s='+CurrFindText);
  if LastFindDialog = nil then exit;
  CurrFindText := LastFindDialog.FindText;
  SearchIsDone := false;

  res := CodePointPos(CurrFindText, Memo1.Text, Memo1.SelStart + 2);
  // try to loop if allowed
  if (res = 0) and not IsFindLoopDenied() then
     res := CodePointPos(CurrFindText, Memo1.Text, 1);
  //DebugLn('         currFindPos='+IntToStr(CurrFindPos));
  if (res > 0) then
  begin
    Memo1.SelStart := res - 1;
    Memo1.SelLength := Length(CurrFindText);
    //Memo1.SetFocus;
  end
  else begin
    showMessage('Cannot find "' + CurrFindText + '"');
    SearchIsDone := true;
  end;
end;

// search the nearest occurrence before Memo1.SelStart
// loop by default, except if denied by FindDialog
procedure TForm1.FindPrev(Sender:TObject);
var _lastValidPos, _temp, _limitPos:integer;
  CurrFindText:string;
begin
  //DebugLn('FindPrev selStart=' + IntToStr(Memo1.SelStart) + ' s='+CurrFindText);
  if LastFindDialog = nil then exit;
  CurrFindText := LastFindDialog.FindText;
  SearchIsDone := false;

  // limitPos is the max position before break
  _limitPos:=Memo1.SelStart + 1;
  // start at the beginning of the file
  _lastValidPos:=0;
  _temp:=0; // will be incremented before first use
  while(true) do begin
  _temp := CodePointPos(CurrFindText, Memo1.Text, _temp + 1);
  // not found after _temp
  if (_temp <= 0) then break;
  // found but after limitPos
  if (_temp >= _limitPos) then begin
    // if a valid position has already been found, leave here
     if (_lastValidPos > 0) then break;
     // otherwise, if allowed, set limit to the end of file
     if IsFindLoopDenied() then break;
     _limitPos := CodePointLength(Memo1.Text);
     // just in case...
     if _temp >= _limitPos then break;
     end;
  // record a valid position found
  _lastValidPos := _temp;
  end;

  if (_lastValidPos > 0) then
  begin
    Memo1.SelStart := _lastValidPos - 1;
    Memo1.SelLength := Length(CurrFindText);
    //Memo1.SetFocus;
  end
  else begin
       showMessage('Cannot find "' + CurrFindText + '"');
       SearchIsDone := true;
  end;
end;

const z_OPEN_SAVE = 0;

procedure TForm1.CmdLineTimerTimer(Sender: TObject);
begin
  	CmdLineTimer.Enabled:=false;
    try
	    loadCommandLineArguments();
	finally
	end;
end;

procedure TForm1.LoadCommandLineArguments;
var _arg, _filename, _filepath:string;
begin
  (* 4 options :
    	filename
        /a filename (a = ansi)
        /w filename (w = utf16)
        /p filename (p = print)
        a, w, p may be uppercase
        *)
	_arg := '';
	if paramCount = 1 then
        _filename := paramstr(1)
    else if paramCount >= 2 then begin
      _arg := paramstr(1);
      _filename := paramstr(2);
      end
    else
    	exit;

    // is it relative or absolute filename ?
    _filepath := extractfilepath(_filename);
    if (length(_filepath) <= 0) then
    	_filename := getCurrentDir() + DirectorySeparator + _filename;

    if not fileexists(_filename) then exit;

	_arg := lowercase(_arg);
    if (_arg = '/a') then
    	openFile(_filename, 0) // ANSI
    else if (_arg = '/w') then
        openFile(_filename, 1) // UTF16 LE
    else if (_arg = '/p') then begin
      	openFile(_filename, -1); // auto-detect encoding
		FilePrint1Execute(nil); // ... then print
    end
    else
    	openFile(_filename, -1); // auto-detect encoding

end;

procedure TForm1.FileNew1Execute(Sender: TObject);
begin
  // anything to save ?
  if not PromptForFileSave(Sender) then exit;

  // clear and reset
  newBlankContent();
  ResetUndo();
end;

procedure TForm1.FileOpen1Execute(Sender: TObject);
var dlg:TWinOpenDialog;
  _filename:string = '';
  i:integer;
  _sel:integer = -1;
begin

    // ensure there is nothing to save
    if not PromptForFileSave(Sender) then exit;

    // ask user for a file to open
    dlg := TWinOpenDialog.Create(Form1);
    try
    with dlg do begin
		DefaultExt := '.txt';
        Filter := 'Text File|*.txt|All Files|*.*';
        FilterIndex := 1;
		Options := [ofFileMustExist,ofEnableSizing,ofViewDetail];
        Title := 'Open existing file';
        // combo
        with ControlData do begin
	        ControlType := COMBOBOX;
	        ControlLabel := 'Encoding:';
	        ComboItems.clear;
	        ComboItems.add('Auto-Detect');
	        for i:=0 to Length(AVAILABLE_ENCODINGS)-1 do
		        ComboItems.add(AVAILABLE_ENCODINGS[i]);
	        ComboSelectedIndex := 0;
		end;

        if Execute then begin
           _filename := Filename;
           _sel := ControlData.ComboSelectedIndex - 1; // -1 because the first index is 'Auto-Detect'
		end;
	end;

	finally
      dlg.Free;
	end;

    if Length(_filename) > 0 then
      	OpenFile(_filename, _sel);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
    canClose := PromptForFileSave(Sender);
end;

procedure TForm1.OpenFile(Filename:string; encodingIndex:integer);
var _filename, s, enc:String;
  p:pchar;
  stream:TMemoryStream;
begin
     // retrieve filename
     _filename:=Filename;
     stream := TMemoryStream.create;
     try
       // load file into a temporary string buffer 's' using a TMemoryStream
       // to get encoding agnostic data (raw bytes only)
	 	stream.loadFromFile(_filename);
        if stream.size > 0 then begin
	        setLength(s, stream.size);
	        stream.ReadBuffer(s[1], stream.size);
			// guess line ending char
	        CurrLineEndingRec:=GetLineEndingRec(s);
            // if the string contains no ending char, set default
            if CurrLineEndingRec.name = 'n.a.' then
            	CurrLineEndingRec := LINE_ENDING_WINDOWS;

	        log('opening file with encodingIdx=' + inttostr(encodingIndex));
	        // set the input encoding : auto-detect or user-specified
	        if (encodingIndex < 0) or (encodingIndex >= Length(LCONV_ENCODINGS)) then begin
		        // guess encoding
				enc:=GuessEncoding(s);
	            // fix a bug in GuessEncoding function for files starting with a NUL char
	            // guessEncoding answers ansi when they are typically utf16
	            if (enc='cp1252') then begin
		             p:=pchar(s);
		             if (byte(p^)=0) then
		                enc:='ucs2be';
	             end;
			end
			else
				enc:= LCONV_ENCODINGS[encodingIndex];
	        // debug
	        //DebugTStringsOptions(memo1.lines);
	        // set memo1 lines options for when it will save data
	        memo1.lines.Options := [soUseLocale, soPreserveBOM];
	        (* doesn't work...
	           TMemo detects correctly any any line ending char
	           and converts all to CRLF whatever LineBreak is set to
	        // set line break for proper data reading
	        memo1.Lines.LineBreak:=CurrLineEndingPattern.seq;
	        *)
	        // load text in the correct format (required for ANSI in particular)
	        memo1.Text := ConvertEncoding(s, enc, EncodingUTF8);
		end
        else begin
            memo1.Text := '';
            enc := 'utf8';
		end;
		// save filename & current encoding for display & save
        ClearBackup();
        SetCurrAndBackupFilenames(_filename);
        FilenameUpdated();
        lConvToAvailableEncoding(enc, CurrEncoding, CurrEncodingIndex);
        // reset undo buffer etc...
        ResetUndo();
        // if the file is a backup file, treat it as a new file with unsaved content
        if CurrFileIsABackupFile then
           ContentHasChanged := true;
        // debug
        //DebugTStringsOptions(memo1.lines);
	 finally
       stream.free();
	 end;
    // debug only
    //DebugLn('encoding='+enc);
    //DebugStringSymbolCodes(memo1.Lines.LineBreak);
	updateStatusBarStatus();
    UpdateStatusBarCaret();
end;

function TForm1.PromptForFileSave(Sender: TObject):boolean;
begin
  result := true;
  if ContentHasChanged then begin
    if FileExists(CurrFilename) then begin
      case Application.MessageBox(
      PCHAR('Do you want to save changes to ' + CurrFilename + ' ?'),
      'NotepadEx',
      MB_YESNOCANCEL) of
      IDYES : FileSave1.Execute();
      IDNO : ClearBackup();
      else
	      result := false;
      end;
	end
	else begin
        if (Length(memo1.lines.text)>0) then
        	case Application.MessageBox(
	      	    PCHAR('Do you want to save changes to Untitled ?'),
	        	'NotepadEx',
	           	MB_YESNOCANCEL) of
           IDYES : FileSaveAs1Execute(Sender);
           IDNO : ClearBackup();
           else
               result := false;
				end;
    end;
  end;
end;

// saves current content to filepath using using CurrEncodingIndex
// returns effective targetEncoding as used in lConv or empty string
function TForm1.SaveFile(Filepath: string; silentErrors: boolean): string;
var stream:TFileStream=nil;
  s:ansistring;
  targetEncoding:string;
begin
  result := '';
  // convert to initial encoding ?
 //DebugLn('saving as ' + CurrEncoding);
 try
   stream := TFileStream.Create(filepath, fmCreate, fmShareExclusive);
 except
    if not silentErrors then
	    Application.MessageBox(pchar(CurrFilename + ' is read-only'), 'NotepadEx', MB_OK);
    exit;
 end;

 try
   (* doesn't work ; TMemo just save lines with CRLF so we need to do it manually
   memo1.Lines.TextLineBreakStyle := CurrLineEndingPattern.LineBreakStyle;
   memo1.Lines.saveToFile(CurrFilename)
   *)
 	  if CurrEncodingIndex in [0..Length(LCONV_ENCODINGS)] then
    	  targetEncoding:=LCONV_ENCODINGS[CurrEncodingIndex]
    else
        targetEncoding:='utf8';
    // first, convert CRLF to the correct format (do it while utf8)
    s := stringreplace(memo1.Text, CHAR_CR + CHAR_LF, CurrLineEndingRec.seq, [rfReplaceAll]);
    // then transcode lines to target encoding
    s := ConvertEncoding(s, EncodingUTF8, targetEncoding, targetEncoding='ansi');
    // save to a memorystream
    if Length(s) > 0 then
	    stream.write(s[1], length(s))
    else
	    stream.write('', 0);

    // return ok
    result := targetEncoding;
 finally
   FreeAndNil(stream);
 end;

end;

procedure TForm1.FileSave1Execute(Sender:TObject);
var
  //stream:TFileStream=nil;
  //s:ansistring;
  targetEncoding:string;
begin
  if (Length(CurrFilename) <= 0) then begin
    FileSaveAs1Execute(Sender);
    exit;
    end;

	targetEncoding := saveFile(CurrFilename, false);

    if Length(targetEncoding) > 0 then begin
		ContentHasChanged := false;
		lConvToAvailableEncoding(targetEncoding, CurrEncoding, CurrEncodingIndex);
		UpdateTitle();
		UpdateStatusBarStatus();

        ClearBackup();
	end;
end;

procedure TForm1.FileSaveAs1Execute(Sender: TObject);
var dlg:TWinSaveDialog;
  i:integer;
begin
  dlg := TWinSaveDialog.Create(Form1);
  with dlg do begin
  try
  	DefaultExt := '.txt';
    Filter := 'Text File|*.txt|All Files|*.*';
    FilterIndex := 1;
  	Options := [ofOverwritePrompt,ofNoReadOnlyReturn,ofEnableSizing,ofViewDetail];
    Title := 'Save file as';

    with ControlData do begin
	    // Encoding combo
	    ControlType := COMBOBOX;
	    ControlLabel := 'Encoding:';
	    ComboItems.clear;
	    for i:=0 to Length(AVAILABLE_ENCODINGS)-1 do
		    ComboItems.add(AVAILABLE_ENCODINGS[i]);
	    ComboSelectedIndex:=CurrEncodingIndex;
	end;

	if Execute then begin
		// set encoding
		CurrEncodingIndex:=ControlData.ComboSelectedIndex;
		CurrEncoding:=AVAILABLE_ENCODINGS[CurrEncodingIndex];
		// we need to set CurrFilename before FileSave1Execute
		// backupFilename must stay the same because it will be cleared
		CurrFilename := Filename;
		FileSave1Execute(Sender); // will clear old backup
		// here we set the new backupFilename
		SetCurrAndBackupFilenames(CurrFilename);
		// notify
		FilenameUpdated();
	end;

  finally
    Free;
  end;
  end;

end;

procedure TForm1.newBlankContent;
begin
  memo1.lines.Clear;
  CurrFilename:='';
  CurrBackupFilename := GetBackupFilename();
  lConvToAvailableEncoding('utf8', CurrEncoding, currEncodingIndex);
  CurrLineEndingRec:=DefaultLineEndingRec();
  FilenameUpdated();
end;

const z_FORM = 0;

function getFontSize(Font:TFont):integer;
begin
if Font.PixelsPerInch > 0 then
	result := round(-GetFontData(Font.Handle).height * 72 / Font.PixelsPerInch)
else
    result := 9;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

// Memo is alLeft only (not alClient) to allow DarkPanel on top of it
 Memo1.Width := Form1.width;

 // backup init
 BackupFolderPath := GetBackupFolderPath();
  if not DirectoryExists(BackupFolderPath) then
  	CreateDir(BackupFolderPath);

  // undo
  MemoUndo := TMemoUndo.create;
  MemoUndo.Memo := Memo1;
  MemoUndo.onHistoryChanged := @OnMemoHistoryChanged;

  Form1.Caption:= ApplicationProperties1.Title;

  // default size
  Form1.Width:=min(round(Screen.Width / 2),1024);
  Form1.Height:=min(round(Screen.Height / 2), 800);

  memo1.lines.Options := [soUseLocale, soPreserveBOM];

  // load stored settings
  reg.ParentKey := REG_KEY;

  // statusbar show/hide
  ViewStatusBarShowHide.Checked := reg.readBool(REG_BOOL_STATUSBAR, DEFAULT_STATUSBAR_ISVISIBLE);
  StatusBar1.Visible:=ViewStatusBarShowHide.Checked;

  // wordwrap
  FormatWordwrap.Checked:=reg.readBool(REG_BOOL_WORDWRAP, DEFAULT_WORDWRAP);
  Memo1.WordWrap:=FormatWordwrap.checked;

  // auto backup
  ExAutoBackupEnabled.Checked := reg.readBool(REG_BOOL_AUTOBACKUP, DEFAULT_AUTOBACKUP);

  // Display selStart isntead of CaretPos
  ExReplaceWithCaretPos.Checked:= reg.readBool(REG_BOOL_SHOW_CARETPOS, DEFAULT_SHOW_CARETPOS);

  // font
  if reg.keyExists(REG_FONT) then
  	reg.readFont(REG_FONT, Memo1.Font);

  // we need a specific function because Memo1.Font default size is 0 (=default system)
  Memo1.Font.size := getFontSize(Memo1.Font);
  // store initial font size as a reference for zooming
  MemoBaseFontSize := Memo1.Font.Size;

  // no reg store - may code it if needed
  MemoScrollbar.ThinnerInactiveThumb:=true;

  // dark theme
  ExIsDarkTheme.Checked:=reg.readBool(REG_BOOL_DARK_THEME, DEFAULT_DARK_THEME);
  SetTheme();

  SetTextMargin();
  newBlankContent();
  ResetUndo();

  // any file to open ?
  if (paramCount >= 1) then
  	// use a timer so that the empty window is visible as soon as possible
  	// and avoid displaying the print dialog without the main window behind
	CmdLineTimer.enabled := true

  else // 'else' is very important to avoid multiple open
      // backup files are open only when the application is opened by the user
      // without any commandLine
      if ExAutoBackupEnabled.Checked then
	  	LoadBackupTimer.Enabled:= true;

  // init status bar
  UpdateStatusBarCaret();
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 try
	 if RestoreMutexHandle <> 0 then begin
 		CloseHandle(RestoreMutexHandle);
	 end;
    finally
      RestoreMutexHandle := 0;
	end;
  FreeAndNil(MemoUndo);
end;

procedure TForm1.FormResize(Sender: TObject);
var fixedWidth, newWidth, i:Integer;
begin
  // Memo1 align is AlLeft with 4 anchors,
  // but we need to react when Windows Display changes (100% -> 150% etc)
  Memo1.Width := Form1.width;
  // set darkpanel width to system default scrollbar width
  DarkPanel.Width:=GetSystemMetrics(SM_CXVSCROLL);
end;

procedure TForm1.StatusBar1Resize(Sender: TObject);
var fixedWidth, newWidth, i:Integer;
begin
  // resize statusbar panels following Notepad statusbar which sticks to the right
  // we resize the first panel : first panel width = total width - other panel widths
  fixedWidth := 0;
  for i:=2 to StatusBar1.Panels.Count-1 do
      fixedWidth += StatusBar1.Panels[i].Width;
  fixedWidth += StatusBar1.Panels[0].Width;
  newWidth := Form1.Width - fixedWidth;
  StatusBar1.Panels[1].Width := newWidth;
end;

const z_UNDO = 0;

  var lastSelStart:integer;
procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
	MemoUndo.OnIdle();

    if Memo1.selStart <> lastSelStart then begin
      //debugln('idle');
      lastSelStart := Memo1.selStart;
      updateStatusBarCaret();
	end;
end;

procedure TForm1.ResetUndo;
begin
	MemoUndo.Reset();
	ContentHasChanged:=false;
	AutoBackupTimer.Enabled:=false;

	UpdateTitle();
	UpdateStatusBarStatus();
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
	// forward event
	MemoUndo.OnMemoChange();

	// reset backup timer
	if ExAutoBackupEnabled.Checked then begin
    	// enable/disable a timer on every key pressed is a huge CPU overhead
    	// to avoid this, we just start the timer if not started, then increment
    	// its 'Tag' property.
    	// when Timer elapses, if Tag > 0 then it restarts
    	// otherwise it saves data to the backup file
    	if not AutoBackupTimer.Enabled then begin
            //debugln('restarting timer');
        	AutoBackupTimer.Enabled:=true;
            AutoBackupTimer.Tag:=0; // we use Tag as a 'hasChanged' property
            UpdateStatusBarStatus();
		end
		else
            AutoBackupTimer.Tag := AutoBackupTimer.Tag + 1;
	end;

    // set hasChanged
    if not ContentHasChanged then begin
	    ContentHasChanged := true;
    	// update title
	    UpdateTitle();
	end;

end;

procedure TForm1.Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
	);
begin
 // catch CTRL+Z & CTRL+Y manually (nightmare with Actionlist)
if (ssCtrl in Shift) then begin
    if (Key = 90) then begin // Z
		Key := 0;
		MemoUndo.Undo();
    end
    else if (Key = 89) then begin // Y
	    Key := 0;
	    MemoUndo.Redo();
	end;
end;
end;

procedure TForm1.Memo1KeyPress(Sender: TObject; var Key: char);
begin
// filter control chars to keep only backspace, tab, CR & LF
 if Ord(Key) < 32 then
   if (Key <> #8) and (Key <> #9) and (Key <> #10) and (Key <> #13) then begin
   	log('!! strange key=' + IntToStr(byte(Key)));
    exit;
   end;
 // forward event
 MemoUndo.OnKeyPress(Key);
end;

procedure TForm1.MenuEditRedo1Click(Sender: TObject);
begin
    MemoUndo.redo;
end;

procedure TForm1.MenuEditUndo1Click(Sender: TObject);
begin
    MemoUndo.Undo;
end;

procedure TForm1.OnMemoHistoryChanged(Sender: TObject);
begin
    UpdateStatusBarStatus();
end;

const z_SETTINGS = 0;

procedure TForm1.ViewStatusBarShowHideExecute(Sender: TObject);
begin
  StatusBar1.Visible:= ViewStatusBarShowHide.Checked;
  reg.WriteBool(REG_BOOL_STATUSBAR, StatusBar1.Visible);
end;

procedure TForm1.FormatWordwrapExecute(Sender: TObject);
begin
  Memo1.WordWrap:= FormatWordwrap.Checked;
  reg.WriteBool(REG_BOOL_WORDWRAP, Memo1.WordWrap);
  // set margin again : changing wordwrap resets windows edit control
  SetTextMargin();
end;

procedure TForm1.ExAutoBackupEnabledExecute(Sender: TObject);
begin
    reg.writeBool(REG_BOOL_AUTOBACKUP, ExAutoBackupEnabled.checked);
    UpdateStatusBarStatus();
end;

procedure TForm1.ExReplaceWithCaretPosExecute(Sender: TObject);
begin
 reg.writeBool(REG_BOOL_SHOW_CARETPOS, ExReplaceWithCaretPos.checked);
 UpdateStatusBarCaret();
end;

procedure TForm1.ExIsDarkThemeExecute(Sender: TObject);
begin
 reg.writeBool(REG_BOOL_DARK_THEME, ExIsDarkTheme.checked);
 SetTheme();
end;

procedure TForm1.FormatFontEdit1Accept(Sender: TObject);
begin
  // store user selected font
	Memo1.Font.Assign(FormatFontEdit1.Dialog.Font);
	reg.writeFont(REG_FONT, Memo1.Font);
	MemoBaseFontSize := Memo1.Font.size;
    // apply current zoom level
	Memo1.Font.Size := round(memoBaseFontSize * CurrZoomLevel / 100);
    // refresh text margin
	SetTextMargin();
end;

procedure TForm1.FormatFontEdit1BeforeExecute(Sender: TObject);
begin
  	// open font dialog with current font
    FormatFontEdit1.Dialog.Font.Assign(Memo1.Font);
    // but the size must be the zoom=100% size, not the current size
    if (currZoomLevel <> 100) then
	    FormatFontEdit1.Dialog.Font.Size := MemoBaseFontSize;
end;

const z_SINGLES = 0;

procedure TForm1.EditInsertDateTimeExecute(Sender: TObject);
var str, _text:String;
  _pos:Integer;
begin
  // string to insert
  str := DateTimeToStr(Now);
  // copy memo text & pos
  _text := Memo1.Text;
  _pos := Memo1.SelStart;
  // insert str into text @ pos
  Insert(str, _text, _pos);
  // assign new text & pos to Memo
  Memo1.Text := _text;
  Memo1.SelStart := _pos + Length(str);
end;

procedure TForm1.AboutAction1Execute(Sender: TObject);
begin
  // show dialog + link to Notepad help file
  with TAboutForm.Create(self) do begin
  	ShowModal();
  	Free;
  end;
end;

procedure TForm1.FilePageSetup1Execute(Sender: TObject);
begin
  PageSetupDialog1.Execute;
end;

procedure TForm1.FilePrint1Execute(Sender: TObject);
begin
  PrintDialog1.Execute;
end;

const z_VISIBLE = 0;

procedure TForm1.ViewZoomInExecute(Sender: TObject);
begin
  CurrZoomLevel += 10;
  if CurrZoomLevel > 500 then
      CurrZoomLevel := 500;
  Memo1.Font.Size := round(memoBaseFontSize * CurrZoomLevel / 100);
  SetTextMargin();
  UpdateStatusBarStatus();
end;

procedure TForm1.ViewZoomOutExecute(Sender: TObject);
begin
  CurrZoomLevel -= 10;
  if CurrZoomLevel < 10 then
      CurrZoomLevel := 10;
  Memo1.Font.Size := round(memoBaseFontSize * CurrZoomLevel / 100);
  SetTextMargin();
  UpdateStatusBarStatus();
end;

procedure TForm1.ViewZoomResetExecute(Sender: TObject);
begin
  CurrZoomLevel := 100;
  Memo1.Font.Size := memoBaseFontSize;
  SetTextMargin();
  UpdateStatusBarStatus();
end;

procedure TForm1.Memo1MouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if (ssCtrl in Shift) then
     ViewZoomOutExecute(Sender);
end;

procedure TForm1.Memo1MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if (ssCtrl in Shift) then
     ViewZoomInExecute(Sender);
end;

procedure TForm1.MemoScrollbarExit(Sender: TObject);
begin
    log('exit');
end;

procedure TForm1.FilenameUpdated;
begin
  // update displayed filename
	CurrTitleFilename := 'Untitled';
	if (Length(CurrFilename) > 0) then
    	CurrTitleFilename := ExtractFileNameOnly(CurrFilename);
	if (currFileIsABackupFile) then
 		CurrTitleFilename := '[BACKUP] ' + ExtractFilenameOnly(CurrBackupFilename);
    // update app title
    UpdateTitle();
    // update memo color
    UpdateAppColors();
end;

procedure TForm1.UpdateStatusBarCaret;
begin
 if ExReplaceWithCaretPos.Checked then
 	StatusBar1.Panels[2].Text := IntToStr(Memo1.selStart)
    	+ '/' + IntToStr(CodePointLength(Memo1.Text))
 else
	 StatusBar1.Panels[2].Text := 'Ln ' + IntToStr(Memo1.CaretPos.Y+1)
   		+ ', Col ' + IntToStr(Memo1.CaretPos.X+1)
end;

procedure TForm1.UpdateStatusBarStatus;
  var fsize:string;
begin
  if (MemoUndo.History.Count <= 0) then
     exit;

  //estMemSize := MemoUndo.History.Count * MemoUndo.History.ItemSize + MemoUndo.History.Last.CumulatedTextSize;

  // panel 0
  StatusBar1.Panels[0].Text:=IntToStr(MemoUndo.CurrentHistoryIndex+1) + '/'
  	+ IntToStr(MemoUndo.History.Count); // + '   ' + BytesToStr(estMemSize);

  // panel 1
  fsize := SizeToStr(Length(Memo1.Text));
  if not ExAutoBackupEnabled.checked then
  	statusBar1.panels[1].Text := fsize + '  - Auto-backup disabled '
  else if AutoBackupTimer.Enabled then
  	statusBar1.panels[1].Text := fsize + '  - ...'
  else if not AutoBackupTimer.Enabled then
    statusBar1.panels[1].Text := fsize + '  - ok';

  // panel 2 is updated in UpdateStatusBarCaret

  // panel 3, 4 & 5
  StatusBar1.Panels[3].Text := IntToStr(CurrZoomLevel) + '%';
  StatusBar1.Panels[4].Text := CurrLineEndingRec.name;
  StatusBar1.Panels[5].Text := CurrEncoding;

  // undo/redo status
  MenuEditUndo1.Enabled:=MemoUndo.CurrentHistoryIndex > 0;
  MenuEditRedo1.Enabled:=MemoUndo.CurrentHistoryIndex<MemoUndo.History.Count-1;
end;

procedure TForm1.UpdateTitle;
var title:string;
begin
	title := CurrTitleFilename;
  if (ContentHasChanged) then
     title := '*' + title;
  title += ' - NotepadEx';
  Application.Title:=title;
  Form1.Caption:=title;
end;

procedure TForm1.SetTextMargin();
var val: Integer;
begin
	val := Round(Memo1.Font.Size / 2);
    // could also use Memo1.BorderSpacing.Left & .Right to stay multiplatform
    Memo1.SetMargins(val, 0);

    if ExIsDarkTheme.Checked then begin
     	// force DarkPanel on top, over Memo1
		SetWindowPos(DarkPanel.Handle, 0, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
	end
	else
		SetWindowPos(Memo1.Handle, 0, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE)

end;

procedure TForm1.UpdateAppColors;
begin
    if CurrFileIsABackupFile then begin
		if ExIsDarkTheme.Checked then begin
	    	Memo1.Color := DARKTHEME_BG_BACKUPFILE;
            Form1.Color:=DARKTHEME_BG_BACKUPFILE;
		end
		else
            Memo1.Color := DEFAULT_BG_BACKUPFILE;
	end
	else begin
    	if ExIsDarkTheme.Checked then
	        Memo1.Color := DARKTHEME_BG_NORMAL
        else
    	    Memo1.Color := DEFAULT_BG_NORMAL;
	end;
	SetTextMargin();
end;

const z_DARK_THEME = 0;

procedure TForm1.SetTheme;
begin
    if ExIsDarkTheme.Checked then begin
       StatusBar1.Visible:=false;
       Form1.Menu := nil;
       Form1.Color := DARKTHEME_BG_NORMAL;
       MemoScrollbar.Memo := Memo1;
       MemoScrollbar.Theme.LoadFromRec(BASICSCROLLBAR_DARK_THEME);
       Memo1.Font.Color := DARKTHEME_FONT;
       Memo1.BorderSpacing.Top := 5;
       Memo1.BorderSpacing.Bottom := 5;
       Memo1.WordWrap:=true; // force wordwrap
       DarkPanel.Width:=GetSystemMetrics(SM_CXVSCROLL);
       DarkPanel.Left:=Form1.Width-DarkPanel.Width;
       DarkPanel.Align:=alRight;
       DarkPanel.Visible:=true;
    end
    else begin
       Form1.Menu := MainMenu1;
       Form1.Color := DEFAULT_BG_NORMAL;
       // optimize Memo1 performance by removing OnScroll handler
       MemoScrollbar.memo := nil;
       Memo1.Font.Color := DEFAULT_FONT;
       Memo1.ScrollBars := ssAutoBoth;
       Memo1.BorderSpacing.Top := 0;
       Memo1.BorderSpacing.Bottom := 0;
       statusBar1.visible := ViewStatusBarShowHide.Checked;
       memo1.WordWrap:=FormatWordwrap.Checked;
       DarkPanel.Visible:=false;
    end;
    UpdateAppColors();
end;

procedure TForm1.ButtonPanelClick(Sender: TObject);
begin
 	// leave Dark mode
    ExIsDarkTheme.Checked:=false;
    ExIsDarkThemeExecute(sender);
end;

procedure TForm1.ButtonPanelMouseEnter(Sender: TObject);
begin
 	// simple & easy button-like user feedback
    ButtonPanel.BevelInner := bvLowered;
end;

procedure TForm1.ButtonPanelMouseLeave(Sender: TObject);
begin
  // simple & easy button-like user feedback
    ButtonPanel.BevelInner := bvNone;
end;

end.

