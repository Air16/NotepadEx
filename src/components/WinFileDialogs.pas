{ 	Windows only
	Adds a combobox or a checkbox to TOpenDialog and TSaveDialog
    using IFileDialogCustomize interface

    Licensed under LGPL
    Copyright Air16 - 2022

    --------------------------------------------------------------

  Example1 : Add an 'Encoding' Combobox to FileOpen

  function TForm1.GetFilenameAndEncodingChoice(var Filename:string; var Encoding:string):boolean;
    var dlg:TWinOpenDialog;
    begin

    dlg := TWinOpenDialog.Create(Form1);

    try

    // this can be set using Object Inspector too
    with dlg.ControlData do begin
      ControlLabel := 'Preferred encoding';
    	ControlType := COMBOBOX;
      ComboItems.Clear;
      ComboItems.Add('UTF8');
      ComboItems.Add('ANSI');
      ComboItems.Add('UTF16-LE');
      // no pre-selection
      ComboSelectedIndex := -1;
    end;

    if not dlg.Execute then exit;

    Result := true;
    Filename := dlg.Filename;
    with dlg.ControlData do
      if ComboSelectedIndex in [0..ComboItems.Count - 1] then
  		encoding := ComboItems[ComboSelectedIndex];

    finally
    	dlg.Free;
    end;
  end;

Example2 : Add a 'Auto-Detect' Checkbox to FileOpen

function TForm1.GetFilenameAndAutoDetectChoice(var Filename:string; var AutoDetect:boolean):boolean;
var dlg:TWinOpenDialog;
begin

	dlg := TWinOpenDialog.Create(Form1);

	try

	// this can be set using Object Inspector too
	with dlg.ControlData do begin
	  ControlLabel := '';
	  ControlType := CHECKBOX;
	  CheckboxLabel := 'Auto-Detect';
	  CheckboxChecked := AutoDetect; // initialized to given param value
	end;

	if not dlg.Execute then exit;

	Result := true;
	Filename := dlg.Filename;
	AutoDetect := dlg.ControlData.CheckboxChecked;

	finally
	end;

end;

}
unit WinFileDialogs;

{$mode ObjFPC}{$H+}

interface

uses
	Classes, Dialogs, LResources;

type

  TWinFileDialogControlType = (NONE, COMBOBOX, CHECKBOX);

  { TWinFileDialogControlData }

  TWinFileDialogControlData = class(TPersistent)
  	protected
    	FType: TWinFileDialogControlType;
        FLabel: string;
        FComboSelectedIndex: Integer;
        FComboItems: TStrings;
        FCheckboxLabel: string;
        FCheckboxChecked: boolean;
        FOnChange: TNotifyEvent;
        procedure SetComboItems(const Value: TStrings); // just enough for lfm file
        procedure Changed;
    public
        procedure Assign(Source : TPersistent); override;
        constructor Create();
        destructor Destroy(); override;
        property OnChange: TNotifyEvent read FOnChange write FOnChange;
    published
        // Type of the control to add to the dialog
        property ControlType:TWinFileDialogControlType read FType write FType;
        // Label of the visual group (common to all control types)
        property ControlLabel: string read FLabel write FLabel;
        // Combo item list
        property ComboItems:TStrings read FComboItems write SetComboItems;
        (* Combo selected index, in [0..ComboItems.Count-1].
         If the value is out of this range, no item is selected.
         set it before calling Execute, read it after Execute to get the user choice *)
        property ComboSelectedIndex: integer read FComboSelectedIndex write FComboSelectedIndex;
        // Checkbox label
        property CheckboxLabel: string read FCheckboxLabel write FCheckboxLabel;
        (* Checkbox checked state
        Set it before calling Execute, read if after to get the user choice *)
        property CheckboxChecked: boolean read FCheckboxChecked write FCheckboxChecked;
  end;

  { TWinOpenDialog }

  TWinOpenDialog = class(TOpenDialog)
   protected
      FControlData:TWinFileDialogControlData;
      function DoExecute:boolean; override;
   public
	constructor Create(AOwner: TComponent); override;
	destructor Destroy; override;
   published
    property ControlData:TWinFileDialogControlData read FControlData write FControlData;
  end;

  { TWinSaveDialog }

  TWinSaveDialog = class(TSaveDialog)
  	protected
      FControlData:TWinFileDialogControlData;
      function DoExecute:boolean; override;
    public
	constructor Create(AOwner: TComponent); override;
	destructor Destroy; override;
   published
    property ControlData:TWinFileDialogControlData read FControlData write FControlData;
  end;

  procedure Register;

implementation

uses
    shlobj;

procedure Register;
begin
  RegisterComponents('Air16 Windows',[TWinOpenDialog, TWinSaveDialog]);
end;

(* adds a control of type 'ControlType' to the dialog with a default value
	and fills ControlId with it
    *)
function beforeExecute(DialogHandle: THandle;
  ControlData:TWinFileDialogControlData; var ControlId:integer):boolean;
const
  GROUP_ID:DWORD = 5000;
  CONTROL_ID:DWORD = 5001;
var c:IFileDialog;
  z:IFileDialogCustomize;
  i:integer;
begin
    Result := false;

    // initial checks
    with ControlData do begin
        // ensure we have a control to display
        if ControlType = NONE then exit;
		// COMBO : ensure we have at least one item to display
        if ControlType = COMBOBOX then
            if ComboItems.Count <= 0 then exit;
	end;

    try
	    // retrieve IFileDialog
		c := IFileDialog(DialogHandle);
		// then ask IFileDialog for IFileDialogCustomize
		if c.queryInterface(IFileDialogCustomize, z) <> S_OK then exit;

	    with ControlData do begin
		    // start a visual group
		    z.StartVisualGroup(GROUP_ID, pWideChar(UTF8Decode(ControlLabel)));
		    // COMBO
		    if ControlType = COMBOBOX then begin
		        // add items
				z.AddComboBox(CONTROL_ID);
				for i:=0 to ComboItems.Count-1 do
					z.AddControlItem(
		            	CONTROL_ID,
		                i,
	                    // Lazarus UTF8 string must be converted to Windows WideString
		                pWideChar(UTF8decode(ComboItems.strings[i])));
		        // set selected item
			    z.SetSelectedControlItem(CONTROL_ID, ComboSelectedIndex);
			end
		    // CHECKBOX
		    else if ControlType = CHECKBOX then begin
		       // add the checkbox
		        z.AddCheckButton(
		        	CONTROL_ID,
		            pWideChar(UTF8decode(CheckboxLabel)),
		            CheckboxChecked);
			end;

		    // close the visual group
			z.EndVisualGroup;
		    // and place it beside OK button
			z.makeProminent(GROUP_ID);

		    // set controlId (will be used in _afterExecute)
		    ControlId := CONTROL_ID;

	        Result := true;
	    end;

	finally
	end;
end;

// retrieves the result into ComboSelectedIndex, CheckboxChecked or EditText
function afterExecute(Handle: THandle;
  ControlData: TWinFileDialogControlData; controlId:integer):boolean;
var c:IFileDialog;
  z:IFileDialogCustomize;
  tempBool:longbool;
  tempInt: integer;
begin
    Result := false;

    try
	// retrieve IFileDialog
	c := IFileDialog(Handle);
	// then ask IFileDialog for IFileDialogCustomize
	if c.queryInterface(IFileDialogCustomize, z) <> S_OK then exit;

    // we have an interface
	Result := true;

    with ControlData do begin
        if ControlType = COMBOBOX then
        begin
            // get selectedIndex
	        ComboSelectedIndex := -1;
	   		z.GetSelectedControlItem(controlId, DWORD(tempInt));
            ComboSelectedIndex := tempInt;
            Result := true;
		end
        else if ControlType = CHECKBOX then begin
            // get checked state (into a Winbool)
        	z.GetCheckButtonState(controlId, tempBool);
            // convert to boolean
            CheckboxChecked := tempBool;
		end
        else
        	// should not happen
        	Result := false;
	end;

	finally
	end;
end;

{ TWinFileDialogControlData }

constructor TWinFileDialogControlData.Create;
begin
    inherited Create;
	FComboItems := TStringList.Create;
end;

procedure TWinFileDialogControlData.Assign(Source: TPersistent);
var src: TWinFileDialogControlData;
begin
    if Source is TWinFileDialogControlData then begin
        src := TWinFileDialogControlData(Source);
        ControlType := src.ControlType;
        ControlLabel := src.ControlLabel;
        ComboItems.Assign(src.ComboItems);
        ComboSelectedIndex := src.ComboSelectedIndex;
        CheckboxChecked := src.CheckboxChecked;
        CheckboxLabel := src.CheckboxLabel;
	end
    else
		inherited Assign(Source);
end;

procedure TWinFileDialogControlData.Changed;
begin
    if Assigned(FOnChange) then
    	FOnChange(self);
end;

destructor TWinFileDialogControlData.Destroy;
begin
    FComboItems.Free;
	inherited Destroy;
end;

// should write a Setter for each field
procedure TWinFileDialogControlData.SetComboItems(const Value: TStrings);
begin
    if Value = FComboItems then exit;
    FComboItems.Assign(Value);
    Changed;
end;

{ TOpenDialogWithCombo }

constructor TWinOpenDialog.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
    FControlData := TWinFileDialogControlData.Create();
end;

destructor TWinOpenDialog.Destroy;
begin
    FControlData.free;
	inherited Destroy;
end;

function TWinOpenDialog.DoExecute: boolean;
var controlId:integer = 0;
  initOk:boolean;
begin
    initOk := beforeExecute(Handle, FControlData, controlId);
    result := inherited DoExecute;
    if initOk then
    	afterExecute(Handle, FControlData, controlId);
end;

{ TSaveDialogWithCombo }

constructor TWinSaveDialog.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
    FControlData := TWinFileDialogControlData.Create();
end;

destructor TWinSaveDialog.Destroy;
begin
    FControlData.Free;
	inherited Destroy;
end;

function TWinSaveDialog.DoExecute: boolean;
var controlId:integer;
  initOk:boolean;
begin
    initOk := beforeExecute(Handle, FControlData, controlId);
    result := inherited DoExecute;
    if initOk then
    	afterExecute(Handle, FControlData, controlId);
end;

initialization
{$I WinFileDialogs.lrs}

end.

