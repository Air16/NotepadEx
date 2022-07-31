{
	EasyRegistry
    Licensed under MIT
    Copyright Air16 2022

    Provides a component named TEasyRegistry that :
    - encapsulates recurring calls to read/write keys in Windows registry
    - provides direct calls to read/write booleans, integers, strings
    - is able to store TFont objects (Windows only)

    The registry access is opened and closed on each get or set call.

    The typical use is the storage of a few application properties.

    If you need to read/write many keys at the same time, it's more efficient
    to work with a TRegistry object : open, then read/write all, then close.
}
unit EasyRegistry;

{$mode ObjFPC}{$H+}

interface

uses
	Classes, SysUtils, Registry, Graphics, LResources;

type

  // enum for RootKey property
  TEasyRegistryRootKey = (
  	HKEY_CLASSES_ROOT,
    HKEY_CURRENT_USER,
	HKEY_LOCAL_MACHINE,
	HKEY_USERS,
	HKEY_PERFORMANCE_DATA,
    HKEY_CURRENT_CONFIG,
	HKEY_DYN_DATA
    );

  { TEasyRegistry }

  TEasyRegistry = class(TComponent)
    protected
      FRootKey:HKEY; // TRegistry HKEY
      _FRootKey: TEasyRegistryRootKey;
      FParentKey:string;
      procedure SetRootKey(val: TEasyRegistryRootKey);
    public
      function readBool(key:string;defValue:boolean = false):boolean;
      procedure writeBool(key:string;value:boolean);
      function readString(key:string;defValue:string = ''):string;
      procedure writeString(key:string;value:string);
      function readInteger(key:string;defValue:Integer = 0):Integer;
      procedure writeInteger(key:string;value:Integer);
      function readFont(key:string; Font:TFont):boolean;
      procedure writeFont(key:string;Font:TFont);
      function keyExists(key:string):boolean;
      function folderExists(folder:string):boolean;
      constructor Create(AOwner:TComponent); override;
    published
      property RootKey:TEasyRegistryRootKey read _FRootKey write SetRootKey;
      property ParentKey:string read FParentKey write FParentKey;
  end;

procedure Register;

implementation

uses
    windows, LCLType;

type
  FontRec = packed record
    Color: TColor;
    LogFont: TLogFont;
  end;


procedure Register;
begin
  RegisterComponents('Air16 Windows',[TEasyRegistry]);
end;

constructor TEasyRegistry.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
end;

function TEasyRegistry.folderExists(folder: string): boolean;
var reg:TRegistry;
begin
  reg := TRegistry.Create();
  result := false;
  try
  reg.RootKey:=FRootKey;
  if reg.OpenKey(FParentKey, false) then
      result := reg.KeyExists(folder);
  finally
    reg.free;
  end;
end;

function TEasyRegistry.keyExists(key: string): boolean;
var reg:TRegistry;
begin
  reg := TRegistry.Create();
  result := false;
  try
  reg.RootKey:=FRootKey;
  if reg.OpenKey(FParentKey, false) then
      result := reg.ValueExists(key);
  finally
    reg.free;
  end;
end;

function TEasyRegistry.readBool(key: string; defValue: boolean): boolean;
var reg:TRegistry;
begin
  reg := TRegistry.Create();
  try
  reg.RootKey:=FRootKey;
  if reg.OpenKey(FParentKey, true) then begin
  	if not reg.valueExists(key) then begin
		reg.createKey(key);
        reg.writeBool(key, defValue);
        end;
    result := reg.readBool(key);
  end;
  finally
    reg.free;
  end;
end;

function TEasyRegistry.readFont(key: string; Font:TFont):boolean;
var
  fRec: FontRec;
  reg:TRegistry;
begin
  result := false;
  reg := TRegistry.create;
  try
  reg.RootKey := FRootKey;
  if reg.OpenKey(FParentKey, False) then
      if reg.ReadBinaryData(key, frec, SizeOf(fRec)) = SizeOf(fRec) then begin
        Font.Handle := CreateFontIndirect(fRec.LogFont);
        Font.Color := fRec.Color;
        end;

  finally
    reg.free;
  end;
end;

function TEasyRegistry.readInteger(key: string; defValue: Integer): Integer;
var reg:TRegistry;
begin
  reg := TRegistry.Create();
  try
  reg.RootKey:=FRootKey;
  if (reg.OpenKey(FParentKey, true)) then begin
  	if not reg.valueExists(key) then begin
		reg.createKey(key);
        reg.writeInteger(key, defValue);
        end;
    result := reg.readInteger(key);
  end;
  finally
    reg.free;
  end;
end;


function TEasyRegistry.readString(key: string; defValue: string): string;
var reg:TRegistry;
begin
  reg := TRegistry.Create();
  try
  reg.RootKey:=FRootKey;
  if (reg.OpenKey(FParentKey, true)) then begin
  	if not reg.valueExists(key) then begin
		reg.createKey(key);
        reg.writeString(key, defValue);
        end;
    result := reg.readString(key);
  end;
  finally
    reg.free;
  end;
end;

procedure TEasyRegistry.SetRootKey(val: TEasyRegistryRootKey);
begin
    case val of
    HKEY_CLASSES_ROOT 	: FRootKey := Registry.HKEY_CLASSES_ROOT;
    HKEY_CURRENT_USER 	: FRootKey := Registry.HKEY_CURRENT_USER;
	HKEY_LOCAL_MACHINE	: FRootKey := Registry.HKEY_LOCAL_MACHINE;
	HKEY_USERS			: FRootKey := Registry.HKEY_USERS;
	HKEY_PERFORMANCE_DATA	: FRootKey := Registry.HKEY_PERFORMANCE_DATA;
    HKEY_CURRENT_CONFIG	: FRootKey := Registry.HKEY_CURRENT_CONFIG;
	HKEY_DYN_DATA		: FRootKey := Registry.HKEY_DYN_DATA;
	end;
    _FRootKey := val;
end;

procedure TEasyRegistry.writeBool(key:string; value:boolean);
var reg:TRegistry;
begin
    reg := TRegistry.Create();
    try
    reg.RootKey:=FRootKey;
    if (reg.OpenKey(FParentKey, true)) then begin
    	if not reg.valueExists(key) then
      		reg.createKey(key);
        reg.writeBool(key, value);
    end;
    finally
      reg.free;
    end;
end;

procedure TEasyRegistry.writeFont(key: string; Font: TFont);
var frec:FontRec;
  reg:TRegistry;
begin
if Windows.GetObject(Font.Handle, SizeOf(fRec.LogFont), @fRec.LogFont) > 0 then
begin
	fRec.Color := Font.Color;
	reg:=TRegistry.Create();
	try
		reg.RootKey:=FRootKey;
		if reg.OpenKey(FParentKey, True) then
			reg.WriteBinaryData(key, fRec, SizeOf(fRec));
	finally
		reg.free;
	end;
end;
end;

procedure TEasyRegistry.writeInteger(key: string; value: Integer);
var reg:TRegistry;
begin
    reg := TRegistry.Create();
    try
    reg.RootKey:=FRootKey;
    if (reg.OpenKey(FParentKey, true)) then begin
    	if not reg.valueExists(key) then
      		reg.createKey(key);
        reg.writeInteger(key, value);
    end;
    finally
      reg.free;
    end;end;

procedure TEasyRegistry.writeString(key: string; value: string);
var reg:TRegistry;
begin
    reg := TRegistry.Create();
    try
    reg.RootKey:=FRootKey;
    if (reg.OpenKey(FParentKey, true)) then begin
    	if not reg.valueExists(key) then
      		reg.createKey(key);
        reg.writeString(key, value);
    end;
    finally
      reg.free;
    end;
end;

initialization
{$I EasyRegistry.lrs}

end.

